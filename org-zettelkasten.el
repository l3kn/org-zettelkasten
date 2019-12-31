(use-package ts)

(require 'org-zk-utils)
(require 'org-zk-macros)
(require 'org-zk-categories)
(require 'org-zk-titlecase)
(require 'org-zk-cache)
(require 'org-zk-xapian)
(require 'org-zk-xapian-query)
(require 'org-zk-links)
(require 'org-zk-hydra)
(require 'org-zk-repeat)
(require 'org-zk-task-list)
(require 'org-zk-calendar)
(require 'org-zk-dashboard)
(require 'org-zk-projects)

(defun org-zk-files-with-titles ()
  "Returns an alist of entries (title . (filename . category))"
  (org-zk-cache-mapcan
   (lambda (filename cache-file)
     (let ((category (oref cache-file category))
           (title (org-zk-cache-get-keyword cache-file "TITLE")))
       (if category
           (list
            (cons
             (format
              "%s (%s)"
              (or title filename)
              (org-zk-category-name category))
             filename)))))))

(defvar org-zk-ivy-histoy nil)

(defun org-zk-select-file (action)
  (ivy-read
   "File: "
   (org-zk-files-with-titles)
   :history 'org-zk-ivy-history
   :action action))

(defun org-zk-open-file ()
  "Select a file, then open it"
  (interactive)
  (org-zk-select-file (lambda (selection) (find-file (cdr selection)))))

(defun org-zk-link-file ()
  "Select a file, then insert an org-mode link to it"
  (interactive)
  (org-zk-select-file
   (lambda (selection)
     (insert (org-zk-make-link (cdr selection) (org-zk-file-title (cdr selection)))))))

(defun org-zk-new-file (&optional insert-link)
  "Create a new org zettelkasten file.
Prompts for a title, and a project, then uses the projects
name-fn to generate a filename."
  (interactive)
  (org-zk-category-prompt
   (lambda (category)
     (let* ((category (cdr category))
            (title (org-zk-read-title))
            (link-fn (org-zk-category-link-fn category))
            (name-fn (org-zk-category-name-fn category))
            (setup-fn (org-zk-category-setup-fn category))
            (name (funcall name-fn title))
            (file (expand-file-name
                   (concat name ".org")
                   (org-zk-category-path category))))
       (if (file-exists-p file)
           (error "Aborting, file already exists: %s" file))
       (when insert-link
         (insert (funcall link-fn file title))
         (save-buffer))
       (find-file file)
       (funcall setup-fn title)
       (save-buffer)))))

(defun org-zk-create-file (title category)
  "Create a new org zettelkasten file given its category and title.
Returns the name of the new file"
  (let* ((link-fn (org-zk-category-link-fn category))
         (name-fn (org-zk-category-name-fn category))
         (setup-fn (org-zk-category-setup-fn category))
         (name (funcall name-fn title))
         (file (expand-file-name
                (concat name ".org")
                (org-zk-category-path category))))
    (if (file-exists-p file)
        (error "Aborting, file already exists: %s" file))
    (with-current-buffer (find-file-noselect file)
      (funcall setup-fn title)
      (save-buffer)
      (kill-buffer))
    file))

(defun org-zk-new-file-and-link ()
  "Create a new org zettelkasten file and insert a link to it at
point."
  (interactive)
  (org-zk-new-file t))

(defun org-zk-copy-link-to-file ()
  "Create an org link to the current file and copy it to the kill-ring"
  (interactive)
  (kill-new (org-zk-make-link (buffer-file-name))))

(defvar org-zk-quick-query-history nil)

(defun org-zk--ivy-query (query action)
  (let ((files (mapcar
                (lambda (entry)
                  (cons
                   (format "%s (%s)"
                           (third entry)
                           (second entry))
                   (first entry)))
                (org-zk-xapian-query query))))
    (cond
     ((> (length files) 1)
      (ivy-read "Note: "
                files
                :action action
                :require-match t
                :history org-zk-quick-query-history))
     ((= (length files) 1) (funcall action (car files)))
     (t (message "No Results")))))

(defun org-zk-quick-query (query)
  (interactive "Mquery: ")
  (org-zk--ivy-query
   query
   (lambda (selection)
     (switch-to-buffer
      (find-file-noselect (cdr selection)))
     (org-zk-skip-headers)
     (search-forward query))))

(defun org-zk-refresh-after-save ()
  (let* ((filename (buffer-file-name))
         (category (org-zk-category-for-file filename)))
    (when (and (equal (file-name-extension filename) "org") category)
      (org-zk-xapian-refresh-file (org-zk-category-name category) filename))))

(defun org-zk-add-ids-to-headlines ()
  "Make sure all headlines in the current file have an ID property"
  (interactive)
  ;; Can't use `org-map-entries' as it opens a prompt when run inside
  ;; a buffer that hasn't been saved yet (Non-existing file / org
  ;; agenda)
  (save-excursion
    (goto-char (point-max))
    (while (outline-previous-heading)
      (org-id-get-create))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'org-zk-add-ids-to-headlines nil 'local)))

;; Index current buffer on focus change
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (org-zk-cache-process-buffer)))

(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (org-zk-cache-process-buffer)))

(defadvice other-frame (before other-frame-now activate)
  (when buffer-file-name (org-zk-cache-process-buffer)))

(if (fboundp 'ace-window)
    (defadvice ace-window (before other-frame-now activate)
      (when buffer-file-name (org-zk-cache-process-buffer))))

(defun org-zk-file-title (file)
  "If FILE is in the org cache, return its title,
if not, return its filename."
  (let ((cache-file (org-zk-cache-get (expand-file-name file))))
    (if cache-file
        (or (org-zk-cache-get-keyword cache-file "TITLE")
            file)
      file)))

(defun org-zk-files-linking-here ()
  "Generate a list of files linking to the current buffer."
  (let ((path (buffer-file-name)))
    (org-zk-cache-filter
     (lambda (source file)
       (--any (string= (oref it path) path)
              (oref file links))))))

(defun org-zk-rename (title)
  "Rename the current zettel, prompting for a new TITLE."
  (interactive (list (org-zk-read-title)))
  (org-zk-keywords-set-or-add "TITLE" title)
  (let ((target (buffer-file-name)))
   (dolist (file (org-zk-files-linking-here))
     (org-zk-in-file (car file)
       (org-zk-update-link target nil title)))))

(provide 'org-zettelkasten)
