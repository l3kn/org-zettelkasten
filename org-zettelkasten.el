(use-package ts)

(require 'org-el-cache)


(require 'org-zk-utils)
(require 'org-zk-macros)
(require 'org-zk-categories)
(require 'org-zk-titlecase)
;; (require 'org-zk-xapian)
;; (require 'org-zk-xapian-query)
(require 'org-zk-links)
(require 'org-zk-hydra)
;; (require 'org-zk-repeat)
;; (require 'org-zk-task-list)
;; (require 'org-zk-calendar)
;; (require 'org-zk-dashboard)
;; (require 'org-zk-projects)
(require 'org-zk-derived-tasks)

(defun org-zk-files-with-titles ()
  "Returns an alist of entries (title . filename)"
  (org-el-cache-mapcan-files
   (lambda (file cache-entry)
     (let ((category (plist-get cache-entry :category))
           (title (or (org-el-cache-get-keyword file "TITLE") file)))
       (if category
           (list
            (cons
             (format "%s (%s)" title (org-zk-category-name category))
             file)))))))

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
     (insert (org-zk-make-link
              (cdr selection)
              (org-zk-file-title (cdr selection)))))))

(defun org-zk-copy-link-to-file ()
  "Create an org link to the current file and copy it to the kill-ring"
  (interactive)
  (kill-new (org-zk-make-link (buffer-file-name))))

(if (fboundp 'ace-window)
    (defadvice ace-window (before other-frame-now activate)
      (when buffer-file-name (org-el-cache-process-buffer))))

(defun org-zk-file-title (file)
  "If FILE is in the org cache, return its title,
if not, return its filename."
  (let ((file (expand-file-name file)))
    (or (org-el-cache-get-keyword file "TITLE") file)))

(defun org-zk-files-linking-here ()
  "Generate a list of files linking to the current buffer."
  (let ((path (buffer-file-name)))
    (org-el-cache-filter-files
     (lambda (file cache-entry)
       (--any (string= (plist-get it :path) path)
              (plist-get cache-entry :links))))))

(defun org-zk-rename (title)
  "Rename the current zettel, prompting for a new TITLE."
  (interactive (list (org-zk-read-title)))
  (org-zk-keywords-set-or-add "TITLE" title)
  (let ((target (buffer-file-name)))
    (dolist (file (org-zk-files-linking-here))
      (org-zk-in-file (car file)
        (org-zk-update-link target nil title)))))

(provide 'org-zettelkasten)
