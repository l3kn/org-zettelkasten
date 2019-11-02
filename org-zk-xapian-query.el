(require 'org-zk-utils)
(require 'org-zk-xapian)

;; Search view
(defun org-zk-xapian-query-buffer ()
  "Return buffer for showing the zettelkasten index"
  (get-buffer-create "*Org Zettelkasten Xapian Query*"))

(defvar org-zk-xapian-query-format
  (vector (list "Title" 40 t)
          (list "Project" 10 t)
          (list "Acc." 5 t)))

(defun org-zk-xapian-query-tabulate (results)
  (mapcar (lambda (result)
            (list
             (first result)
             (vector
              (third result)
              (second result)
              (format "%.2f" (fourth result)))))
          results))

(defvar org-zk-xapian-query-query "")

(defun org-zk-xapian-query-run-query (query &optional other-buffer)
  "Run a query and display the results. If OTHER-BUFFER is non-nil,
show the query index in a new / the other buffer"
  (setq org-zk-xapian-query-query query)
  (let ((results (org-zk-xapian-query query)))
    (with-current-buffer (org-zk-xapian-query-buffer)
      (setq tabulated-list-format org-zk-xapian-query-format)
      (setq org-zk-xapian-query-query query)
      (org-zk-xapian-query-mode)
      (tabulated-list-init-header)
      (setq tabulated-list-entries (org-zk-xapian-query-tabulate results))
      (setq tabulated-list-sort-key (cons "Acc." 'flip))
      (tabulated-list-print)
      (if other-buffer
          (switch-to-buffer-other-window (current-buffer))
        (switch-to-buffer (current-buffer))))))

(defun org-zk-xapian-query-open-file ()
  (interactive)
  (let ((file (tabulated-list-get-id)))
    (switch-to-buffer
     (find-file-noselect file))
     (org-zk-skip-headers)
     (search-forward org-zk-xapian-query-query nil t)))

(defun org-zk-xapian-query-new-query (query)
  (interactive (list (read-string "query: ")))
  (org-zk-xapian-query-run-query query))

(defun org-zk-xapian-query-edit-query (query)
  (interactive (list (read-string "query: " org-zk-xapian-query-query)))
  (org-zk-xapian-query-run-query query))

(defvar org-zk-xapian-query-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'org-zk-xapian-query-open-file)
    (define-key map (kbd "/") 'org-zk-xapian-query-new-query)
    (define-key map (kbd "s") 'org-zk-xapian-query-new-query)
    (define-key map (kbd "e") 'org-zk-xapian-query-edit-query)
    (define-key map (kbd "c") 'zk-new-file)
    map)
  "Key bindings for the org xapian query index mode")

(define-derived-mode org-zk-xapian-query-mode tabulated-list-mode "Org Xapian Query"
  "Extension of tabulated-list mode for searching for files with xapian")

(provide 'org-zk-xapian-query)
