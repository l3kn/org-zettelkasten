(require 'org-zk-repeat)

;; TODO: Implement as headline hook
(defun org-zk-calendar-view--time-entries ()
  (org-el-cache-mapcan-headlines
   (lambda (_cached-file headline)
     (let ((entries nil))
       (if-let ((deadline (plist-get headline :deadline)))
           (push (plist-put deadline :headline headline)
                 entries))
       (if-let ((scheduled (plist-get headline :scheduled)))
           (push (plist-put scheduled :headline headline)
                 entries))
       (setq entries
             (nconc entries
                    (mapcar (lambda (timestamp)
                              (plist-put timestamp :headline headline))
                            (plist-get headline :timestamps))))
       entries))))

(defvar org-zk-calendar-view-n-days 14)

(defface org-zk-calendar-view-today-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face to highlight entries for the current day"
  :group 'org-zk-calendar-view)

(defun org-zk-calendar-view--repeated-time-entries (n-days)
  "Generate a list of all entries with a timestamp,
including repetitions of timestamps.
Returns a list of elements (headline ts type)."
  (mapcan
   (lambda (entry)
     (if (equal (plist-get (plist-get entry :headline) :style) "habit")
         (if-let ((next (org-zk-repeat-repetition-next entry)))
             (list (plist-put entry :repetition next)))
       (mapcar
        (lambda (repetition) (plist-put entry :repetition repetition))
        (org-zk-repeat-repetitions-next-n-days
         entry
         n-days))))
   (org-zk-calendar-view--time-entries)))

(defun org-zk-calendar-view-buffer ()
  (get-buffer-create "*org-zettelkasten Calendar*"))

(setq org-zk-calendar-view-format
      (vector
       (list "Date" 20 t)
       (list "Type" 10 t)
       (list "File" 20 t)
       (list "Title" 20 t)))

(defun org-zk-calendar-view--ts-format (ts)
  (if ts
      (if (and (ts-hour ts) (ts-minute ts))
          (ts-format "%Y-%m-%d %H:%M" ts)
        (ts-format "%Y-%m-%d" ts))
    "----"))

(defun org-zk-calendar-view-tabulate (entries)
  (mapcar
   (lambda (entry)
     (let* ((headline (plist-get entry :headline))
            (file (plist-get headline :file)))
       (list
        entry
        (vector
         (org-zk-calendar-view--ts-format (plist-get entry :repetition))
         (symbol-name (plist-get entry :type))
         ;; TODO: Find title
         (or (org-el-cache-file-keyword file "TITLE")
             file)
         (plist-get headline :title)))))
   entries))

(defun org-zk-calendar-view-open ()
  (interactive)
  (let* ((headline (plist-get (tabulated-list-get-id) :headline)))
    (find-file (plist-get headline :file))
    (goto-char (plist-get headline :begin))))

(setq org-zk-calendar-view-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map tabulated-list-mode-map)
        (define-key map (kbd "RET") 'org-zk-calendar-view-open)
        map))

(define-derived-mode org-zk-calendar-view-mode tabulated-list-mode "org-zk Calendar"
  "Major mode for listing org calendar entries"
  (hl-line-mode))

(defun org-zk-calendar-view ()
  (interactive)
  (let ((entries (org-zk-calendar-view--repeated-time-entries org-zk-calendar-view-n-days)))
    (with-current-buffer (org-zk-calendar-view-buffer)
      (setq tabulated-list-format org-zk-calendar-view-format)
      (org-zk-calendar-view-mode)
      (tabulated-list-init-header)
      (setq tabulated-list-entries (org-zk-calendar-view-tabulate entries))
      (setq tabulated-list-sort-key (cons "Date" nil))
      (setq tabulated-list-printer #'org-zk-calendar-view-print-entry)
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(defun org-zk-calendar-view--today-p (ts)
  (let ((now (ts-now)))
    (and
     (eq (ts-year ts) (ts-year now))
     (eq (ts-month ts) (ts-month now))
     (eq (ts-day ts) (ts-day now)))))

(defun org-zk-calendar-view--face (entry)
  (let ((ts (plist-get entry :repetition)))
    (if (org-zk-calendar-view--today-p ts) 'bold 'default)))

;; TODO: Move this to the tabulate function
(defun org-zk-calendar-view-print-entry (id cols)
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
     (org-zk-calendar-view--face id))))

(provide 'org-zk-calendar-view)
