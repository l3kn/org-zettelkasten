(require 'ts)

(require 'org-el-cache)

(def-org-el-cache-headline-hook headline-timestamps (element)
  `(:deadline ,(org-zk-time-from-element
                (org-element-property :deadline element)
                'deadline)
              :scheduled ,(org-zk-time-from-element
                           (org-element-property :scheduled element)
                           'scheduled)
              :timestamps ,(org-zk-time--headline-timestamps element)
              :clocks ,(org-element-map
                           element
                           'clock
                         'org-zk-clock-from-element)))

(defclass org-zk-clock ()
  ((begin :initarg :begin)
   (end :initarg :end)
   (status :initarg :status)))

(defun org-zk-clock-from-element (element)
  (let ((value (org-element-property :value element)))
    (if (eq (org-element-property :status element) 'closed)
        (make-instance
         'org-zk-clock
         :begin (org-zk-time--ts-from-element value)
         :end (org-zk-time--ts-from-element-end value)
         :status 'closed)
      (make-instance
         'org-zk-clock
         :begin (org-zk-time--ts-from-element value)
         :end nil
         :status 'running))))

(defmethod org-zk-clock-duration ((clock org-zk-clock))
  "Duration of CLOCK in seconds."
  (ts-diff
   (or (oref clock end) (ts-now))
   (oref clock begin)))

(defmethod org-zk-clock-duration-in-range ((clock org-zk-clock) from to)
  "Duration of CLOCK in seconds."
  (let* ((begin (oref clock begin))
         (end (or (oref clock end) (ts-now)))
         (begin-cut (if (ts<= begin from) from begin))
         (end-cut (if (ts>= end to) to end))
         ;; With the way begin-cut and end-cut work, timestamps fully
         ;; outside the range from-to are "flipped" so that end <
         ;; begin
         (diff (ts-diff end-cut begin-cut)))
    (if (plusp diff) diff 0)))

(defun org-zk-clocking-total (begin end)
  "Time total in the time from BEGIN to END."
  (ts-human-duration
   (org-el-cache-reduce-headlines
    (lambda (_parent headline acc)
      (+ (loop for clock in (plist-get headline :clocks) summing
               (org-zk-clock-duration-in-range clock begin end))
         acc))
    0)))

(defun org-zk-clocks-in-range (begin end)
   (org-el-cache-filter-headlines
    (lambda (_parent headline)
      (plusp (loop for clock in (plist-get headline :clocks) summing
                   (org-zk-clock-duration-in-range clock begin end))))))

(defun org-zk-clocking-total-day ()
  (org-zk-clocking-total
   (org-zk-time-beginning-of-day)
   (org-zk-time-end-of-day)))

(defun org-zk-clocking-total-week ()
  (org-zk-clocking-total
   (ts-adjust 'day -6 (org-zk-time-beginning-of-day))
   (org-zk-time-end-of-day)))

(defun org-zk-clocking-total-month ()
  (org-zk-clocking-total
   (ts-adjust 'day -29 (org-zk-time-beginning-of-day))
   (org-zk-time-end-of-day)))

(provide 'org-zk-clocking)
