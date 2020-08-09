(require 'org-zk-time)

(defun org-zk-awk-titles ()
  (org-zk-parse-titles
   (shell-command-to-string
    "org-files | xargs awk -f /home/leon/src/org-zettelkasten/awk/titles.awk")))

(defun org-zk-parse-titles (titles)
  (mapcar
   (lambda (line) (split-string line "\t"))
  (split-string titles "\n" 'omit-nulls)))

(defun org-zk-awk-clocks ()
  (org-zk-parse-clocks
   (shell-command-to-string
    "org-files-with-archives | xargs awk -f /home/leon/src/org-zettelkasten/awk/clocks.awk")))

(defun org-zk-parse-clocks (clocks)
  (mapcar
   (lambda (line)
     (cl-destructuring-bind (file title headline begin end) (split-string line "\t")
       (list
        :file file
        :title title
        :headline headline
        ;; :begin (org-parse-time-string begin)
        ;; :end (unless (string= end "") (org-parse-time-string end))
        :begin (ts-parse-org begin)
        :end (unless (string= end "") (ts-parse-org end))
        )))
   (split-string clocks "\n" 'omit-nulls)))

(defun org-zk-duration-in-range (clock from to)
  "Duration of CLOCK in seconds."
  (let* ((begin (plist-get clock :begin))
         (end (or (plist-get clock :end) (ts-now)))
         (begin-cut (if (ts<= begin from) from begin))
         (end-cut (if (ts>= end to) to end))
         ;; With the way begin-cut and end-cut work, timestamps fully
         ;; outside the range from-to are "flipped" so that end <
         ;; begin
         (diff (ts-diff end-cut begin-cut)))
    (if (plusp diff) diff 0)))

(defun org-zk-clocking-total (clocks begin end)
  (ts-human-duration
   (loop for clock in clocks summing
         (org-zk-duration-in-range clock begin end))))

(defun org-zk-clocking-overview ()
  (let ((clocks (org-zk-awk-clocks))
        (bod (org-zk-time-beginning-of-day))
        (eod (org-zk-time-end-of-day)))
    (list
     :day (org-zk-clocking-total clocks bod eod)
     :week (org-zk-clocking-total
            clocks
            (ts-adjust 'day -6 bod)
            eod)
     :month (org-zk-clocking-total
            clocks
            (ts-adjust 'day -29 bod)
            eod))))

(provide 'org-zk-awk)
