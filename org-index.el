(setq org-index-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "p") 'org-projects)
    (define-key map (kbd "n") 'org-next-tasks)
    map))

(define-derived-mode org-index-mode special-mode "Org Index"
  "Major mode providing an overview of org-zettelkasten queries"
  ;; (set (make-local-variable 'revert-buffer-function) #'fc-main-view)
  (setq-local cursor-type nil))

(defun org-index--buffer ()
    (get-buffer-create "Org Index"))

(defun org-index--display ()
  (with-current-buffer (org-index--buffer)
    (setq inhibit-read-only t)
    (erase-buffer)
    (insert
     (propertize "Org Index\n\n" 'face 'org-level-1))
    (insert
     (propertize "  [p] Projects\n" 'face 'org-level-1))
    (insert
     (propertize "  [n] Next Tasks\n" 'face 'org-level-1))
    (insert
     (propertize "  [d] Today\n" 'face 'org-level-1))))

(defun org-index ()
  (interactive)
  (org-index--display)
  (switch-to-buffer (org-index--buffer))
  (org-index-mode))
