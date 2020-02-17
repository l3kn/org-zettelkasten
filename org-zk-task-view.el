(require 'org-zk-query)

;;; Font Faces

(defface org-zk-task-view-effort-face
  '((((class color) (background light)) (:foreground "#aaa"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used for dates in the file view."
  :group 'org-zk)

;;; Tabulated List Mode

(defvar org-zk-task-view-format
  (vector
   '("Tag" 8 t)
   '("P" 1 t)
   '("Project" 20 t)
   '("Effort" 6 t)
   '("Title" 40 t)))

(defun org-zk-task-view--sort-todo-keyword (kw)
  (cond
   ((string= kw "NEXT") 6)
   ((string= kw "TODO") 5)
   ((string= kw "WAITING") 4)
   ((string= kw "DONE") 3)
   ((string= kw "DELEGATED") 2)
   ((string= kw "CANCELLED") 1)
   (t 0)))

;; TODO: Use default priority variable
(defun org-zk-task-view--sort-priority (prio)
  (cond
   ((eql prio ?A) 3)
   ((eql prio ?B) 2)
   ((eql prio ?C) 1)
   (t 0)))

(defvar org-zk-task-view--sort-predicates
  (list
   (list (lambda (e) (org-zk-task-view--sort-todo-keyword (plist-get e :todo-keyword))) #'> #'<)
   (list (lambda (e) (org-zk-task-view--sort-priority
                 (or (plist-get e :priority) org-default-priority))) #'> #'<)
   ;; (list (lambda (e) (org-el-cache-get-keyword (oref e parent) "TITLE")) #'string> #'string<)
   (list (lambda (e) (plist-get e :title)) #'string> #'string<)))

;; Keyword
;; Prio
;; Title
(cl-defun org-zk-task-view--sort-predicate (a b &optional (predicates org-zk-task-view--sort-predicates))
  (message "sorting")
  (if (null predicates)
      nil
    (destructuring-bind (transformer pred1 pred2) (car predicates)
      (let ((va (funcall transformer a))
            (vb (funcall transformer b)))
        (cond
         ((funcall pred1 va vb) t)
         ((funcall pred2 va vb) nil)
         (t (org-zk-task-view--sort-predicate a b (cdr predicates))))))))

(defun org-zk-task-view--sort (headlines)
  (sort headlines #'org-zk-task-view--sort-predicate))

(defun org-zk-task-view-tabulate-title (hl)
  (if (null (plist-get hl :tags))
      (propertize
       (plist-get hl :title)
       'face 'org-zk-file-view-title-face)
    (concat
     (propertize
      (plist-get hl :title)
      'face 'org-zk-file-view-title-face)
     " ("
     (mapconcat
      (lambda (tag) (propertize tag 'face 'org-zk-file-view-keyword-face))
      (plist-get hl :tags) ",")
     ")")))

(defun org-zk-task-view-tabulate-todo-keyword (kw)
  (if (null kw)
      ""
      (propertize (substring-no-properties kw) 'face (org-get-todo-face kw))))

(defun org-zk-task-view-tabulate-priority (p)
  (cond
   ((equal p org-highest-priority)
    (propertize (format "%c" p) 'face 'bold))
   ;; ((equal p org-lowest-priority)
   ;;  (propertize (format "%c" p) 'face 'italic))
   (t (format "%c" p))))

;; Works on a list of (file-entry . hl) pairs
(defun org-zk-task-view-tabulate (file-hls)
  (mapcar
   (lambda (file-hl)
     (list
      file-hl
      (vector
       (org-zk-task-view-tabulate-todo-keyword
        (plist-get (cdr file-hl) :todo-keyword))
       (org-zk-task-view-tabulate-priority
        (or (plist-get (cdr file-hl) :priority) org-default-priority))
       (plist-get (car file-hl) :title)
       (propertize
        (or (plist-get (cdr file-hl) :effort) "")
        'face 'org-zk-task-view-effort-face)
       (org-zk-task-view-tabulate-title (cdr file-hl)))))
   file-hls))

(defun org-zk-task-view-buffer ()
  (get-buffer-create "org-zk Tasks"))

(defun org-zk-task-view-show (file-query hl-query)
  (let ((file-hls
         (org-zk-task-view-headlines
          (org-zk-query file-query org-zk-query-file-predicates)
          (org-zk-query hl-query org-zk-query-hl-predicates))))
    (with-current-buffer (org-zk-task-view-buffer)
      (org-zk-task-view-mode)
      (setq tabulated-list-format org-zk-task-view-format)
      (tabulated-list-init-header)
      (setq tabulated-list-entries (org-zk-task-view-tabulate file-hls))
      (setq tabulated-list-sort-key nil)
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(define-derived-mode org-zk-task-view-mode tabulated-list-mode "org-zk Tasks"
  "Major mode for listing org tasks"
  (hl-line-mode))

(setq org-zk-task-view-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map tabulated-list-mode-map)
        (define-key map (kbd "RET") 'org-zk-task-view-open)
        (define-key map (kbd "e") 'org-zk-task-view-set-effort)
        (define-key map (kbd "t") 'org-zk-task-view-set-todo)
        (define-key map (kbd "p") 'org-zk-task-view-set-priority)
        map))

(defun org-zk-task-view-open ()
  (interactive)
  (let ((file-hl (tabulated-list-get-id)))
    (find-file (plist-get (car file-hl) :file))
    (goto-char (plist-get (cdr file-hl) :begin))))

(defun org-zk-task-view-set-effort ()
  (interactive)
  (let* ((headline (tabulated-list-get-id))
         (file (plist-get headline :file))
         (cur (plist-get headline :effort))
         (allowed (org-property-get-allowed-values nil org-effort-property))
         (effort (org-quickselect-effort-prompt cur allowed)))
    (tabulated-list-set-col "Effort" effort)
    ;; TODO: Use in-file macro
    (org-zk-in-file file
      (goto-char (plist-get headline :begin))
      (org-set-effort nil effort)
      (save-buffer))
    ;; FIXME, Hacky re-rendering of the updated list
    (let ((p (point)))
      (org-zk-next-tasks)
      (goto-char p))))

(defun org-zk-task-view-set-todo ()
  (interactive)
  (let* ((headline (tabulated-list-get-id))
         (file (plist-get headline :file)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (plist-get headline :begin))
      (org-todo)
      (save-buffer))
    ;; FIXME, Hacky re-rendering of the updated list
    (let ((p (point)))
      (org-zk-next-tasks)
      (goto-char p))))

(defun org-zk-task-view-headlines (file-pred hl-pred)
  (let (headlines)
    (org-el-cache-each
     org-zk-cache
     (lambda (filename entry)
       (if (funcall file-pred filename entry)
           (dolist (hl (plist-get entry :headlines))
             (if (funcall hl-pred filename hl)
                 (push (cons entry hl) headlines))))))
    headlines))

(org-zk-task-view-show "s:active" "t:NEXT")

(provide 'org-zk-task-view)
