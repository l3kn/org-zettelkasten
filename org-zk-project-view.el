(require 'org-el-cache)
(require 'org-zk-query)

;;; Font Faces

(defface org-zk-project-view-state-face
  '((((class color) (background light)) (:foreground "#aaa"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used for the state in the project view."
  :group 'org-zk)

(defface org-zk-project-view-count-face
  '((((class color) (background light)) (:foreground "#aa0"))
    (((class color) (background dark))  (:foreground "#ff0")))
  "Face used for task counts in the project view."
  :group 'org-zk)

;;; Rest

(defun org-zk-project-view-buffer ()
  (get-buffer-create "*org-zk Projects*"))

(setq org-zk-project-view-format
  (vector
   (list "State" 12 t)
   (list "NEXT" 4 t)
   (list "TODO" 4 t)
   (list "Title" 40 t)))

(defun org-zk-project-view-count-todo-keyword (entry keyword)
  (let ((headlines (plist-get entry :headlines)))
    (count-if
     (lambda (hl) (string= (plist-get hl :todo-keyword) keyword))
     headlines)))

(defun org-zk-project-view-tabulate (entries)
  (mapcar
   (lambda (entry)
     (let ((keywords (plist-get entry :org-keywords)))
       (list
        entry
        (vector
         (propertize
          (org-zk-entry-gtd-state entry)
          'face 'org-zk-project-view-state-face)
         (propertize
          (number-to-string (org-zk-project-view-count-todo-keyword entry "NEXT"))
          'face 'org-zk-project-view-count-face)
         (propertize
          (number-to-string (org-zk-project-view-count-todo-keyword entry "TODO"))
          'face 'org-zk-project-view-count-face)
         (org-zk-file-view-tabulate-title entry)))))
   entries))

(define-derived-mode org-zk-project-view-mode tabulated-list-mode "org-zk Projects"
  "Major mode for listing org gtd projects"
  (hl-line-mode))

(defun org-zk-project-view-blocked-p (entry)
  (zerop (org-zk-project-view-count-todo-keyword entry "NEXT")))

(defun org-zk-project-view-filter-blocked (projects)
  (--filter (not (org-zk-project-view-blocked-p it))
            projects))

(defun org-zk-project-view-show (query)
  (interactive)
  (setq org-zk-project-view-filter query)
  (let* ((pred (org-zk-query query org-zk-query-file-predicates))
         (files (org-el-cache-select org-zk-cache
                 (lambda (filename entry)
                   (and (org-zk-project-p filename entry)
                        (funcall pred filename entry))))))
    (with-current-buffer (org-zk-project-view-buffer)
      (setq tabulated-list-format org-zk-project-view-format)
      (org-zk-project-view-mode)
      (tabulated-list-init-header)
      (setq tabulated-list-entries (org-zk-project-view-tabulate files))
      (setq tabulated-list-sort-key (cons "State" nil))
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(defvar org-zk-project-view-filter ""
  "Current filter of the file view")

(defun org-zk-project-view (query)
  (interactive (list (read-string "Filter: " org-zk-project-view-filter)))
  (org-zk-project-view-show query))

(defun org-zk-project-view-active ()
  (interactive)
  (org-zk-project-view-show "s:active"))

(defun org-zk-project-view-open ()
  "Open the file for the project under point"
  (interactive)
  (find-file (plist-get (tabulated-list-get-id) :file)))

(setq org-zk-project-view-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map tabulated-list-mode-map)
        (define-key map (kbd "RET") 'org-zk-project-view-open)
        (define-key map (kbd "S") 'org-zk-project-view)
        (define-key map (kbd "s") 'org-zk-project-view)
        (define-key map (kbd "k") 'org-zk-file-view-edit-keywords)
        map))

(provide 'org-zk-project-view)
