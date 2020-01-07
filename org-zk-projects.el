(require 'org-zk-cache)

(defvar org-zk-gtd-states
  '("active"
    "someday"
    "planning"
    "cancelled"
    "done"))

(defvar org-zk-default-file-priority "B")
(defvar org-zk-default-file-priorities '("A" "B" "C"))
(defvar org-zk-default-file-state "none")

(defun org-zk-projects-all ()
  (org-zk-cache-file-query
   `(or ,@(--map `(gtd-state ,it) org-zk-gtd-states))))

(define-org-zk-cache-file-query-macro gtd-state (state)
  `(keyword "GTD_STATE" ,state))

;; TODO: Generate these using a macro

(defun org-zk-projects-active ()
  (org-zk-cache-file-query '(gtd-state "active")))

(defun org-zk-projects-someday ()
  (org-zk-cache-file-query '(gtd-state "someday")))

(defun org-zk-projects-planning ()
  (org-zk-cache-file-query '(gtd-state "planning")))

(defun org-zk-projects-cancelled ()
  (org-zk-cache-file-query '(gtd-state "cancelled")))

(defun org-zk-projects-done ()
  (org-zk-cache-file-query '(gtd-state "done")))

(defun org-zk-projects-buffer ()
  (get-buffer-create "*Org Projects*"))

(defvar org-zk-projects-format
  (vector
   (list "State, Pri" 12 t)
   (list "Title" 30 t)
   (list "NEXT" 4 t)
   (list "TODO" 4 t)))

(defun org-zk-projects-count-next (cached-file)
  (length (org-zk-cache-file-headline-query cached-file '(todo "NEXT"))))

(defun org-zk-projects-count-todo (cached-file)
  (length (org-zk-cache-file-headline-query cached-file '(todo "TODO"))))

(defun org-zk-projects-tabulate (cached-files)
  (mapcar
   (lambda (file)
     (list
      (car file)
      (vector
       (concat
        (org-zk-cache-get-keyword (cdr file) "GTD_STATE" org-zk-default-file-state)
        " "
        (org-zk-cache-get-keyword (cdr file) "GTD_PRIORITY" org-zk-default-file-priority))
       (org-zk-cache-file-title (cdr file))
       (number-to-string (org-zk-projects-count-next (cdr file)))
       (number-to-string (org-zk-projects-count-todo (cdr file))))))
   cached-files))

(define-derived-mode org-zk-projects-mode tabulated-list-mode "Org Projects"
  "Major mode for listing org gtd projects"
  (hl-line-mode))

(defun org-zk-projects-blocked-p (cached-file)
  (= 0 (org-zk-projects-count-next cached-file)))

(defun org-zk-projects-filter-blocked (projects)
  (--filter (not (org-zk-projects-blocked-p (cdr it)))
            projects))

(defun org-zk-projects ()
  (interactive)
  (let ((projects (org-zk-projects-filter-blocked (org-zk-projects-all))))
    (with-current-buffer (org-zk-projects-buffer)
      (setq tabulated-list-format org-zk-projects-format)
      (org-zk-projects-mode)
      (tabulated-list-init-header)
      (setq tabulated-list-entries (org-zk-projects-tabulate projects))
      (setq tabulated-list-sort-key (cons "State, Pri" nil))
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(defun org-zk-projects-open ()
  "Open the file for the project under point"
  (interactive)
  (find-file (tabulated-list-get-id)))

(defun org-zk-projects-set-gtd-priority ()
  "Open the file for the project under point"
  (interactive)
  (org-zk-in-file (tabulated-list-get-id)
    (call-interactively 'org-zk-set-gtd-priority)))

(setq org-zk-projects-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map tabulated-list-mode-map)
        (define-key map (kbd "RET") 'org-zk-projects-open)
        (define-key map (kbd ",") 'org-zk-projects-set-gtd-priority)
        map))

(provide 'org-zk-projects)
