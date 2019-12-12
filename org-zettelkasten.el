(use-package ts)

(require 'org-zk-utils)
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

(defun org-zk-new-file-and-link ()
  "Create a new org zettelkasten file and insert a link to it at
point."
  (interactive)
  (org-zk-new-file t))

(defun org-zk-copy-link-to-file ()
  "Create an org link to the current file and copy it to the kill-ring"
  (interactive)
  (kill-new (org-zk-make-link (buffer-file-name))))

(defun org-zk-edit-keywords ()
  "Edit the value of the KEYWORDS keyword of the current buffer."
  (interactive)
  (let* ((options (org-option-parse))
         (keywords (assoc "KEYWORDS" options)))
    (if keywords
        (org-option-apply
         (rest keywords)
         (lambda (kw) (string-trim (read-string "keywords: " kw))))
      (org-option-add
       "KEYWORDS"
       (string-trim (read-string "keywords: "))
       (if (null options)
           (point-min)
         (fourth (first options)))))))

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

(provide 'org-zettelkasten)
