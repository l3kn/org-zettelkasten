(setq org-zk-task-list-format
      (vector
       '("Tag" 8 t)
       '("P" 1 t)
       '("Project" 20 t)
       '("Title" 40 t)
       '("Effort" 6 t)
       '("Tags" 20 t)))

(defun org-zk-task-list--sort-todo-keyword (kw)
  (cond
   ((string= kw "NEXT") 6)
   ((string= kw "TODO") 5)
   ((string= kw "WAITING") 4)
   ((string= kw "DONE") 3)
   ((string= kw "DELEGATED") 2)
   ((string= kw "CANCELLED") 1)
   (t 0)))

;; TODO: Use default priority variable
(defun org-zk-task-list--sort-priority (prio)
  (cond
   ((eql prio ?A) 3)
   ((eql prio nil) 2)
   ((eql prio ?B) 2)
   ((eql prio ?C) 1)
   (t 0)))

(defvar org-zk-task-list--sort-predicates
  (list
   (list (lambda (e) (org-zk-task-list--sort-todo-keyword (plist-get e :todo-keyword))) #'> #'<)
   (list (lambda (e) (org-zk-task-list--sort-priority (plist-get e :priority))) #'> #'<)
   ;; (list (lambda (e) (org-el-cache-get-keyword (oref e parent) "TITLE")) #'string> #'string<)
   (list (lambda (e) (plist-get e :title)) #'string> #'string<)))

;; Keyword
;; Prio
;; Title
(cl-defun org-zk-task-list--sort-predicate (a b &optional (predicates org-zk-task-list--sort-predicates))
  (message "sorting")
  (if (null predicates)
      nil
    (destructuring-bind (transformer pred1 pred2) (car predicates)
      (let ((va (funcall transformer a))
            (vb (funcall transformer b)))
        (cond
         ((funcall pred1 va vb) t)
         ((funcall pred2 va vb) nil)
         (t (org-zk-task-list--sort-predicate a b (cdr predicates))))))))

(defun org-zk-task-list--sort (headlines)
  (sort headlines #'org-zk-task-list--sort-predicate))

(defun org-zk-task-list-tabulate (headlines)
  (mapcar
   (lambda (headline)
     (list
      headline
      (vector
       (substring-no-properties (plist-get headline :todo-keyword))
       (format "%c" (or (plist-get headline :priority) ?B))
       (or (org-el-cache-file-keyword (plist-get headline :file) "TITLE") "")
       (plist-get headline :title)
       (or (plist-get headline :effort) "")
       (mapconcat #'substring-no-properties
                  (plist-get headline :tags)
                  ":"))))
   headlines))

(defun org-zk-task-list-buffer ()
  (get-buffer-create "Org Tasks"))

(defun org-zk-task-list-show (headlines)
  (message "Headlines %d" (length headlines))
  (setq headlines (org-zk-task-list--sort headlines))
  (message "Headlines 2 %d" (length headlines))
  (with-current-buffer (org-zk-task-list-buffer)
    (org-zk-task-list-mode)
    (setq tabulated-list-format org-zk-task-list-format)
    (tabulated-list-init-header)
    (setq tabulated-list-entries (org-zk-task-list-tabulate headlines))
    (setq tabulated-list-sort-key nil)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))


(define-derived-mode org-zk-task-list-mode tabulated-list-mode "Org Tasks"
  "Major mode for listing org tasks"
  (hl-line-mode))

(setq org-zk-task-list-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map tabulated-list-mode-map)
        (define-key map (kbd "RET") 'org-zk-task-list-open)
        (define-key map (kbd "e") 'org-zk-task-list-set-effort)
        (define-key map (kbd "t") 'org-zk-task-list-set-todo)
        (define-key map (kbd "p") 'org-zk-task-list-set-priority)
        map))

(defun org-zk-task-list-open ()
  (interactive)
  (let ((headline (tabulated-list-get-id)))
    (find-file (plist-get headline :file))
    (goto-char (plist-get headline :begin))))

(defun org-zk-task-list-set-effort ()
  (interactive)
  (let* ((headline (tabulated-list-get-id))
         (file (plist-get headline :file))
         (cur (plist-get headline :effort))
         (allowed (org-property-get-allowed-values nil org-effort-property))
         (effort (org-quickselect-effort-prompt cur allowed)))
    (tabulated-list-set-col "Effort" effort)
    ;; TODO: Use in-file macro
    (with-current-buffer (find-file-noselect file)
      (goto-char (plist-get headline :begin))
      (org-set-effort effort)
      (save-buffer))
    ;; FIXME, Hacky re-rendering of the updated list
    (let ((p (point)))
      (org-zk-next-tasks)
      (goto-char p))))

(defun org-zk-task-list-set-todo ()
  (interactive)
  (let* ((headline (tabulated-list-get-id))
         (file (plist-get headline :file)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (plist-get headline :begin))
      (org-todo)
      (save-buffer))
    ;; FIXME, Hacky re-rendering of the updated list
    (let ((p (point)))
      (org-next-tasks)
      (goto-char p))))

(defun org-zk-task-list-set-priority ()
  (interactive)
  (let* ((headline (tabulated-list-get-id))
         (parent (plist-get headline :parent))
         (path (plist-get parent :path)))
    (with-current-buffer (find-file-noselect path)
      (goto-char (plist-get headline :begin))
      (org-priority 'set)
      (save-buffer))
    ;; FIXME, Hacky re-rendering of the updated list
    (let ((p (point)))
      (org-next-tasks)
      (goto-char p))))

(defun org-zk-next-tasks ()
  (interactive)
  (org-zk-task-list-show
   (org-el-cache-filter-headlines
    (lambda (cached-file cached-hl)
      (and (string= (org-el-cache-entry-keyword cached-file "GTD_STATE") "active")
           (member (org-el-cache-entry-property cached-hl :todo-keyword)
                   '("NEXT")))))))

(provide 'org-zk-task-list)
