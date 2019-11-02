(require 'org-zk-calendar)
(require 'org-zk-clocking)

(defun org-zk-dashboard-buffer ()
  (get-buffer-create "*Org Zettelkasten Dashboard*"))

(defvar org-zk-dashboard--ewoc nil)

(defface org-zk-scheduled-face
  '((t . (:foreground "dark orange")))
  "Face to highlight scheduled entries"
  :group 'org-zk-dashboard)

(defface org-zk-deadline-face
  '((t . (:foreground "dark red")))
  "Face to highlight deadline entries"
  :group 'org-zk-dashboard)

(defface org-zk-plain-face
  '((t . (:foreground "black")))
  "Face to highlight normal entries"
  :group 'org-zk-dashboard)

(defun org-zk-dashboard--pp-calendar-entry (entry)
  (insert "   ")
  (message "%s" (third entry))
  (insert
   (case (third entry)
     (deadline (propertize
                (oref (first entry) title)
                'face
                'org-zk-deadline-face))
     (scheduled (propertize
                 (oref (first entry) title)
                 'face
                 'org-zk-scheduled-face))
     (plain (propertize
             (oref (first entry) title)
             'face
             'org-zk-plain-face)))))

(defun org-zk-dashboard-goto-next ()
  (interactive)
  (ewoc-goto-next org-zk-dashboard--ewoc 1))

(defun org-zk-dashboard-goto-prev ()
  (interactive)
  (ewoc-goto-prev org-zk-dashboard--ewoc 1))

(defun org-zk-dashboard-open ()
  (interactive)
  (let ((entity (ewoc-locate org-zk-dashboard--ewoc)))
    (if entity
        (let* ((headline (first (ewoc-data entity)))
               (parent (oref headline parent))
               (path (oref parent path)))
          (find-file path)
          (goto-char (oref headline begin))))))

(setq org-zk-dashboard-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map tabulated-list-mode-map)
        (define-key map (kbd "c") 'org-zk-calendar)
        (define-key map (kbd "t") 'org-zk-next-tasks)
        (define-key map (kbd "j") 'org-zk-projects)
        (define-key map (kbd "RET") 'org-zk-dashboard-open)
        (define-key map (kbd "n") 'org-zk-dashboard-goto-next)
        (define-key map (kbd "p") 'org-zk-dashboard-goto-prev)
        map))

(define-derived-mode org-zk-dashboard-mode special-mode "Org Zettelkasten Dashboard"
  "Major mode for showing a org zettelkasten dashboard"
  (hl-line-mode))

(defun org-zk-dashboard ()
  (interactive)
  (with-current-buffer (org-zk-dashboard-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((ewoc (ewoc-create
                   #'org-zk-dashboard--pp-calendar-entry
                   "  Calendar"
                   )))
        (setq org-zk-dashboard--ewoc ewoc)
        (dolist
            (entry (org-zk-calendar--repeated-time-entries 1))
          (ewoc-enter-last ewoc entry))
        (insert "Org Zettelkasten Dashboard")
        (insert "\n\n")
        (insert "  [c] Full Calendar\n")
        (insert "  [t] Next Tasks\n")
        (insert "  [j] Projects\n")
        (insert "\n")
        (insert "  Clocking, Today:\n")
        (let ((total (org-zk-clocking-total)))
          (insert (format "    %2d:%02d\n"
                   (plist-get total :hours)
                   (plist-get total :minutes))))
        (insert "  Clocking, 7 Days:\n")
        (let ((total (org-zk-clocking-total-week)))
          (insert (format "    %d:%02d\n"
                          (+ (* 24 (plist-get total :days))
                             (plist-get total :hours))
                   (plist-get total :minutes))))
        (insert "\n")
        (org-zk-dashboard-mode)
        (switch-to-buffer (current-buffer))))))

(provide 'org-zk-dashboard)
