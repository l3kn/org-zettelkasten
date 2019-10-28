(require 'org-cache)

(define-org-cache-file-query-macro gtd-state (state)
  `(keyword "GTD_STATE" ,state))

(defvar org-projects-gtd-states
  '("active"
    "on_hold" ; was active or is ready to go, now is paused
    "someday" ; incubator / someday / maybe
    "blocked" ; blocked by some other project
    "planning" ; planning / brainstorming
    "done"
    ))

(defun org-projects-set-gtd-state (state)
  (interactive
   (list (ivy-completing-read "State: " org-projects-gtd-states)))
  (let ((options (org-buffer-options)))
    (set-org-option options "GTD_STATE" state)))

(defun org-projects-all ()
  (org-cache-file-query
   '(or (gtd-state "active")
        (gtd-state "on_hold")
        (gtd-state "blocked")
        (gtd-state "planning")
        (gtd-state "someday"))))

(defun org-projects-active ()
  (org-cache-file-query '(gtd-state "active")))

(defun org-projects-on-hold ()
  (org-cache-file-query '(gtd-state "on_hold")))

(defun org-projects-someday ()
  (org-cache-file-query '(gtd-state "someday")))

(defun org-projects-blocked ()
  (org-cache-file-query '(gtd-state "blocked")))

(defun org-projects-planning ()
  (org-cache-file-query '(gtd-state "planning")))

(defun org-projects-buffer ()
  (get-buffer-create "*Org Projects*"))

(defvar org-projects-format
  (vector
   (list "State" 10 t)
   (list "Title" 30 t)
   (list "NEXT" 4 t)
   (list "TODO" 4 t)))

(defun org-projects-count-next (cached-file)
  (length (org-cache-file-headline-query cached-file '(todo "NEXT"))))

(defun org-projects-count-todo (cached-file)
  (length (org-cache-file-headline-query cached-file '(todo "TODO"))))

(defun org-projects-tabulate (cached-files)
  (mapcar
   (lambda (file)
     (list
      (car file)
      (vector
       (org-cache-get-keyword (cdr file) "GTD_STATE")
       (org-cache-get-keyword (cdr file) "TITLE")
       (number-to-string (org-projects-count-next (cdr file)))
       (number-to-string (org-projects-count-todo (cdr file))))))
   cached-files))

(define-derived-mode org-projects-mode tabulated-list-mode "Org Projects"
  "Major mode for listing org gtd projects"
  (hl-line-mode))

(defun org-projects ()
  (interactive)
  (let ((projects (org-projects-all)))
    (with-current-buffer (org-projects-buffer)
      (setq tabulated-list-format org-projects-format)
      (org-projects-mode)
      (tabulated-list-init-header)
      (setq tabulated-list-entries (org-projects-tabulate projects))
      (setq tabulated-list-sort-key (cons "State" nil))
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(defun org-projects-open ()
  "Open the file for the project under point"
  (interactive)
  (find-file (tabulated-list-get-id)))

(setq org-projects-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map tabulated-list-mode-map)
        (define-key map (kbd "RET") 'org-projects-open)
        map))

(provide 'org-projects)
