(require 'org-zk-cache)

(defvar org-zk-projects-gtd-states
  '("active"
    "on_hold" ; was active or is ready to go, now is paused
    "someday" ; incubator / someday / maybe
    "blocked" ; blocked by some other project
    "planning" ; planning / brainstorming
    "cancelled" ; planning / brainstorming
    "done"))

(defun org-zk-projects-set-gtd-state (state)
  (interactive
   (list (ivy-completing-read "State: " org-zk-projects-gtd-states)))
  (let ((options (org-buffer-options)))
    (set-org-option options "GTD_STATE" state)))

(defun org-zk-projects-all ()
  (org-zk-cache-file-query
   `(or ,@(--map `(gtd-state ,it) org-zk-projects-gtd-states))))

(define-org-zk-cache-file-query-macro gtd-state (state)
  `(keyword "GTD_STATE" ,state))

;; TODO: Generate these using a macro

(defun org-zk-projects-active ()
  (org-zk-cache-file-query '(gtd-state "active")))

(defun org-zk-projects-on-hold ()
  (org-zk-cache-file-query '(gtd-state "on_hold")))

(defun org-zk-projects-someday ()
  (org-zk-cache-file-query '(gtd-state "someday")))

(defun org-zk-projects-blocked ()
  (org-zk-cache-file-query '(gtd-state "blocked")))

(defun org-zk-projects-planning ()
  (org-zk-cache-file-query '(gtd-state "planning")))

(defun org-zk-projects-buffer ()
  (get-buffer-create "*Org Projects*"))

(defvar org-zk-projects-format
  (vector
   (list "State" 10 t)
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
       (org-zk-cache-get-keyword (cdr file) "GTD_STATE")
       (org-zk-cache-get-keyword (cdr file) "TITLE")
       (number-to-string (org-zk-projects-count-next (cdr file)))
       (number-to-string (org-zk-projects-count-todo (cdr file))))))
   cached-files))

(define-derived-mode org-zk-projects-mode tabulated-list-mode "Org Projects"
  "Major mode for listing org gtd projects"
  (hl-line-mode))

(defun org-zk-projects ()
  (interactive)
  (let ((projects (org-zk-projects-all)))
    (with-current-buffer (org-zk-projects-buffer)
      (setq tabulated-list-format org-zk-projects-format)
      (org-zk-projects-mode)
      (tabulated-list-init-header)
      (setq tabulated-list-entries (org-zk-projects-tabulate projects))
      (setq tabulated-list-sort-key (cons "State" nil))
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(defun org-zk-projects-open ()
  "Open the file for the project under point"
  (interactive)
  (find-file (tabulated-list-get-id)))

(setq org-zk-projects-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map tabulated-list-mode-map)
        (define-key map (kbd "RET") 'org-zk-projects-open)
        map))

(provide 'org-zk-projects)
