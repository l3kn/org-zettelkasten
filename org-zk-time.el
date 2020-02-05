(defun org-zk-time--headline-timestamps (element)
  (mapcar
   (lambda (el) (org-zk-time-from-element el 'plain))
   (remove-if-not
    (lambda (e)
      (or (eq (org-element-property :type e) 'active)
          (eq (org-element-property :type e) 'active-range)))
    (org-element-map
        (assoc 'section (org-element-contents element))
        'timestamp
      #'identity))))

(defun org-zk-time-beginning-of-day ()
  (let ((now (ts-now)))
    (make-ts
     :year (ts-year now) :month (ts-month now) :day (ts-day now)
     :hour 0 :minute 0 :second 0)))

(defun org-zk-time-end-of-day ()
  (ts-adjust 'day 1 'second -1 (org-zk-time-beginning-of-day)))

(defun org-zk-time--ts-from-element (el)
  (make-ts
   :year (org-element-property :year-start el)
   :month (org-element-property :month-start el)
   :day (org-element-property :day-start el)
   :hour (or (org-element-property :hour-start el) 0)
   :minute (or (org-element-property :minute-start el) 0)
   :second 0))

(defun org-zk-time--ts-from-element-end (el)
  (make-ts
   :year (org-element-property :year-end el)
   :month (org-element-property :month-end el)
   :day (org-element-property :day-end el)
   :hour (or (org-element-property :hour-end el) 0)
   :minute (or (org-element-property :minute-end el) 0)
   :second 0))

(defun org-zk-time-from-element (el type)
  (if el
      `(:ts ,(org-zk-time--ts-from-element el)
        :type ,type
        :unit ,(org-element-property :repeater-unit el)
        :value ,(org-element-property :repeater-value el))))

(provide 'org-zk-time)
