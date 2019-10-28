(require 'org-zk-repeat)

(defun org-zk-calendar--file-time-entries (cache-file)
  (let ((entries nil))
    (dolist (headline (oref cache-file headlines))
      (let ((deadline (oref headline deadline))
            (scheduled (oref headline scheduled))
            (timestamps (oref headline timestamps)))
        (if deadline
            (push (cons headline deadline) entries))
        (if scheduled
            (push (cons headline scheduled) entries))
        (setq entries
              (nconc entries
                     (mapcar (lambda (timestamp) (cons headline timestamp))
                             timestamps)))))
    entries))

(defun org-zk-calendar--time-entries ()
  "Generate a list of all entries with a timestamp.
Returns a list of elements (headline . org-cache-timestamp)"
  (let ((entries nil))
    (maphash
     (lambda (key value)
       (setq entries (nconc entries (org-zk-calendar--file-time-entries value))))
     org-cache--table)
    entries))

(defvar org-zk-calendar-n-days 14)

(defface org-zk-calendar-today-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face to highlight entries for the current day"
  :group 'org-zk-calendar)

(defun org-zk-calendar--repeated-time-entries ()
  "Generate a list of all entries with a timestamp,
including repetitions of timestamps.
Returns a list of elements (headline ts type)."
  (mapcan
   (lambda (entry)
     (if (equal (oref (car entry) style) "habit")
         (let ((next (org-zk-repeat-repetition-next (cdr entry))))
           (if next
               (list
                (list (car entry) next (oref (cdr entry) type)))
             nil))
       (mapcar
        (lambda (repetition)
          (list (car entry) repetition (oref (cdr entry) type)))
        (org-zk-repeat-repetitions-next-n-days
         (cdr entry)
         org-zk-calendar-n-days))))
   (org-zk-calendar--time-entries)))

(defun org-zk-calendar-buffer ()
  (get-buffer-create "*Org Zettelkasten Calendar*"))

(setq org-zk-calendar-format
  (vector
   (list "Date" 20 t)
   (list "Type" 10 t)
   (list "Title" 30 t)))

(defun org-zk-calendar--ts-format (ts)
  (if ts
      (if (and (ts-hour ts) (ts-minute ts))
          (ts-format "%Y-%m-%d %H:%M" ts)
        (ts-format "%Y-%m-%d" ts))
    "----"))

(defun org-zk-calendar-tabulate (entries)
  (mapcar
   (lambda (entry)
     (list
      entry
      (vector
       (org-zk-calendar--ts-format (second entry))
       (symbol-name (third entry))
       (oref (first entry) title))))
   entries))

(defun org-zk-calendar-open ()
  (interactive)
  (let* ((headline (first (tabulated-list-get-id)))
         (parent (oref headline parent))
         (path (oref parent path)))
    (find-file path)
    (goto-char (oref headline begin))))

(setq org-zk-calendar-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map tabulated-list-mode-map)
        (define-key map (kbd "RET") 'org-zk-calendar-open)
        map))

(define-derived-mode org-zk-calendar-mode tabulated-list-mode "Org Zettelkasten Calendar"
  "Major mode for listing org calendar entries"
  (hl-line-mode))

(defun org-zk-calendar ()
  (interactive)
  (let ((entries (org-zk-calendar--repeated-time-entries)))
    (with-current-buffer (org-zk-calendar-buffer)
      (setq tabulated-list-format org-zk-calendar-format)
      (org-zk-calendar-mode)
      (tabulated-list-init-header)
      (setq tabulated-list-entries (org-zk-calendar-tabulate entries))
      (setq tabulated-list-sort-key (cons "Date" nil))
      (setq tabulated-list-printer #'org-zk-calendar-print-entry)
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(defun org-zk-calendar--today-p (ts)
  (let ((now (ts-now)))
    (and
     (eq (ts-year ts) (ts-year now))
     (eq (ts-month ts) (ts-month now))
     (eq (ts-day ts) (ts-day now)))))

(defun org-zk-calendar--face (id)
  (let ((ts (second id)))
    (if (org-zk-calendar--today-p ts) 'bold 'default)))

(defun org-zk-calendar-print-entry (id cols)
  "Insert a Tabulated List entry at point.
This is the default `tabulated-list-printer' function.  ID is a
Lisp object identifying the entry to print, and COLS is a vector
of column descriptors."
  (let ((beg   (point))
	      (x     (max tabulated-list-padding 0))
	      (ncols (length tabulated-list-format))
	      (inhibit-read-only t))
    (if (> tabulated-list-padding 0)
	      (insert (make-string x ?\s)))
    (let ((tabulated-list--near-rows ; Bind it if not bound yet (Bug#25506).
           (or (bound-and-true-p tabulated-list--near-rows)
               (list (or (tabulated-list-get-entry (point-at-bol 0))
                         cols)
                     cols))))
      (dotimes (n ncols)
        (setq x (tabulated-list-print-col n (aref cols n) x))))
    (insert ?\n)
    ;; Ever so slightly faster than calling `put-text-property' twice.
    (add-text-properties
     beg (point)
     `(tabulated-list-id ,id tabulated-list-entry ,cols))
    (put-text-property
     beg (point)
     'face
     (org-zk-calendar--face id))))

(provide 'org-zk-calendar)
