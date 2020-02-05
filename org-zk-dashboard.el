(require 'org-zk-calendar)
;; (require 'org-zk-clocking)

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
        (define-key map (kbd "c") 'org-zk-agenda-list)
        (define-key map (kbd "t") 'org-zk-todo-list)
        (define-key map (kbd "j") 'org-zk-projects)
        (define-key map (kbd "RET") 'org-zk-dashboard-open)
        (define-key map (kbd "n") 'org-zk-dashboard-goto-next)
        (define-key map (kbd "p") 'org-zk-dashboard-goto-prev)
        map))

(define-derived-mode org-zk-dashboard-mode special-mode "Org Zettelkasten Dashboard"
  "Major mode for showing a org zettelkasten dashboard"
  (hl-line-mode))

(defun org-zk-format-time (time)
  (format "%2d:%02d"
          (+
           (* 24 (plist-get time :days))
           (plist-get time :hours))
          (plist-get time :minutes)))

(defun org-zk-dashboard ()
  (interactive)
  (with-current-buffer (org-zk-dashboard-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((inbox-count (org-zk-inbox-count)))
        (insert (propertize "Org Zettelkasten Dashboard\n" 'face 'org-level-1))
        (when (plusp inbox-count)
          (insert "\n")
          (insert (propertize (format "  Inbox: %d\n" inbox-count) 'face 'org-level-1)))
        (insert "\n")
        ;; (let ((day (org-zk-clocking-total-day))
        ;;       (week (org-zk-clocking-total-week))
        ;;       (month (org-zk-clocking-total-month)))
        ;;   (insert (propertize "  Clocking\n" 'face 'org-level-1))
        ;;   (insert (format "    1 day:  %s\n" (org-zk-format-time day)))
        ;;   (insert (format "    7 day:  %s\n" (org-zk-format-time week)))
        ;;   (insert (format "   30 day:  %s\n" (org-zk-format-time month))))
        ;; (insert "\n")
        (insert "  [c] Full Calendar\n")
        (insert "  [t] Next Tasks\n")
        (insert "  [j] Projects\n")
        (insert "\n")
        (insert "  [q] Quit\n")
        (insert "\n")
        (org-zk-dashboard-mode)
        (switch-to-buffer (current-buffer))))))

(provide 'org-zk-dashboard)
