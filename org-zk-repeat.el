(require 'ts)
(require 'org-zk-clocking)

(defun org-zk-repeat--ts-to-absolute (ts)
  (calendar-absolute-from-gregorian
   (list
    (ts-month ts)
    (ts-day ts)
    (ts-year ts))))

(defun org-zk-repeat--ts-combine-with-absolute (ts abs)
  "Set the date of TS to the one encoded in ABS"
  (destructuring-bind (month day year) (calendar-gregorian-from-absolute abs)
    (make-ts
     :year year
     :month month
     :day day
     :hour (ts-hour ts)
     :minute (ts-minute ts)
     :second (ts-minute ts))))

(defun org-zk-repeat--ts-now ()
  (let ((now (ts-now)))
    (make-ts
     :year (ts-year now)
     :month (ts-month now)
     :day (ts-day now)
     :hour 0
     :minute 0
     :second 0)))

(defun org-zk-repeat-next (now ts unit value)
  "Get the next repetition of TS, relative to time 00:00 today."
  (let* ((abs-ts (org-zk-repeat--ts-to-absolute ts))
         (abs-now (org-zk-repeat--ts-to-absolute now)))
    (if (ts>= ts now)
        ts
      (pcase unit
        ('hour (error "Not implemented yet"))
        ((or 'day 'week)
         (let* ((value (if (eq unit 'week) (* 7 value) value))
                (abs-last (+ abs-ts (* value (/ (- abs-now abs-ts) value))))
                (abs-next (+ abs-last value))
                (ts-last (org-zk-repeat--ts-combine-with-absolute ts abs-last))
                (ts-next (org-zk-repeat--ts-combine-with-absolute ts abs-next)))
           ;; FIXME: Hacky fix for value = 1 repeats
           (if (ts>= ts-last now) ts-last ts-next)))
        ;; FIXME: Doesn't work for cases where current day < number of
        ;; days in target month
        ('month
         (let ((ts-next
                (make-ts
                 :year (ts-year now)
                 :month (ts-month now)
                 :day (ts-day ts)
                 :hour 0
                 :minute 0
                 :second 0)))
           (if (ts>= ts-next now)
               ts-next
               (ts-adjust 'month 1 ts-next))))
        ('year
         (let ((ts-next
                (make-ts
                 :year (ts-year now)
                 :month (ts-month ts)
                 :day (ts-day ts)
                 :hour 0
                 :minute 0
                 :second 0)))
           (if (ts>= ts-next now)
               ts-next
               (ts-adjust 'year 1 ts-next))))))))

(defmethod org-zk-repeat-repetitions-in-range ((timestamp org-zk-time) from to)
  "Generate a list of all repetitions of TS between FROM and TO.
Hourly repetitions are *not* supported.  When using this, no
assumptions should be made about the order of the results"
  (let* ((ts (plist-get timestamp :ts))
         (unit (plist-get timestamp :unit))
         (value (plist-get timestamp :value))
         (ts-next (org-zk-repeat-next from ts unit value))
         (results (list)))
    (while (ts<= ts-next to)
      (push ts-next results)
      (setq
       ts-next
       (org-zk-repeat-next (ts-adjust 'day 1 ts-next) ts unit value)))
    results))

(defmethod org-zk-repeat-repetitions-in-range ((timestamp org-zk-time) from to)
  (list timestamp))

  (defun org-zk-repeat-repetitions-next-n-days (timestamp n-days)
    "Generate a list of all repetitions of TIMESTAMP in the next N-DAYS days.
Hourly repetitions are *not* supported.  When using this, no
assumptions should be made about the order of the results.
Returns a list of *ts* timestamps, not org-zk-clocking-timestamps"
    (let* ((from (org-zk-repeat--ts-now))
           (to (ts-adjust 'day n-days 'minute -1 from)))
      (if (plist-get timestamp :unit)
          (org-zk-repeat-repetitions-in-range timestamp from to)
        (let ((ts (plist-get timestamp :ts)))
          (if (and (ts>= ts from) (ts<= ts to))
              (list ts)
            (list))))))

(defun org-zk-repeat-repetition-next (timestamp)
  (let ((now (org-zk-repeat--ts-now))
        (ts (plist-get timestamp :ts)))
    (if (plist-get timestamp :unit)
        (org-zk-repeat-next now
         ts
         (plist-get timestamp :unit)
         (plist-get timestamp :value))
      (if (ts>= ts now)
          ts
        nil))))

(provide 'org-zk-repeat)
