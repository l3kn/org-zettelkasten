(require 'org-zk-cache)

(defun org-zk-clocking-beginning-of-day ()
  (let ((now (ts-now)))
    (make-ts
     :year (ts-year now)
     :month (ts-month now)
     :day (ts-day now)
     :hour 0
     :minute 0
     :second 0)))

(defun org-zk-clocking-end-of-day ()
  (let ((now (ts-now)))
    (make-ts
     :year (ts-year now)
     :month (ts-month now)
     :day (ts-day now)
     :hour 23
     :minute 59
     :second 59)))

(defun org-zk-clocking-total-day ()
  (let ((total 0))
    (maphash
     (lambda (key value)
       (dolist (hl (oref value headlines))
         (dolist (cl (oref hl clocks))
           (incf
            total
            (org-zk-cache-clock-duration-in-range
             cl
             (org-zk-clocking-beginning-of-day)
             (org-zk-clocking-end-of-day))))))
     org-zk-cache--table)
    (ts-human-duration total)))

(defun org-zk-clocking-total-week ()
  (let ((total 0))
    (maphash
     (lambda (key value)
       (dolist (hl (oref value headlines))
         (dolist (cl (oref hl clocks))
           (incf
            total
            (org-zk-cache-clock-duration-in-range
             cl
             (ts-adjust 'day -6 (org-zk-clocking-beginning-of-day))
             (org-zk-clocking-end-of-day))))))
     org-zk-cache--table)
    (ts-human-duration total)))

(defun org-zk-clocking-total-month ()
  (let ((total 0))
    (maphash
     (lambda (key value)
       (dolist (hl (oref value headlines))
         (dolist (cl (oref hl clocks))
           (incf
            total
            (org-zk-cache-clock-duration-in-range
             cl
             (ts-adjust 'day -29 (org-zk-clocking-beginning-of-day))
             (org-zk-clocking-end-of-day))))))
     org-zk-cache--table)
    (ts-human-duration total)))

(defun org-zk-clocking-open ()
  (let (headlines)
    (maphash
     (lambda (key value)
       (dolist (hl (oref value headlines))
         (dolist (cl (oref hl clocks))
           (if (or (null (oref cl end))
                   (eq (oref cl status) 'active))
               (push hl headlines)))))
     org-zk-cache--table)
    headlines))

(mapcar (lambda (h) (oref h title)) (org-zk-clocking-open))

(provide 'org-zk-clocking)
