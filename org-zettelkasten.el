(require 'org-zk-categories)
(require 'org-zk-titlecase)
(require 'org-cache)
(require 'org-xapian)
(require 'org-xapian-query)
(require 'org-edges)
(require 'org-hydra)
(require 'org-zk-repeat)
(require 'org-task-list)
(require 'org-zk-calendar)

(defun org-zk-files-with-titles ()
  "Returns an alist of entries (title . (filename . category))"
  (org-cache-mapcan
   (lambda (filename cache-file)
     (let ((category (oref cache-file category))
           (title (org-cache-get-keyword cache-file "TITLE")))
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
  (interactive)
  (org-zk-select-file (lambda (selection) (find-file (cdr selection)))))

(provide 'org-zettelkasten)
