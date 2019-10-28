(use-package org-ql)

(defun org-scheduler-files ()
  (mapcar
   #'car
   (org-cache-file-query '(keyword "GTD_STATE" "active"))))

(defun org-scheduler-headlines ()
  (mapcar
   (lambda (headline) (oref headline title))
   (org-cache-headline-query
    '(keyword "GTD_STATE" "active")
    '(or (todo "NEXT")))))

(defun org-scheduler-next-tasks ()
  (org-ql-select
    (org-scheduler-files)
    '(todo "NEXT")))

(defun random-list-element (list)
  (nth (random (length list)) list))

(defun org-scheduler-random-next-task ()
  (random-list-element (org-scheduler-next-tasks)))

(defun org-scheduler-next-tasks ()
  (interactive)
  (org-ql-select
    (org-scheduler-files)
    '(or (todo "NEXT"))))

(length (org-scheduler-next-tasks))
(length (org-scheduler-headlines))
;; 324 NEXT
;; ...
;; 173 NEXT

;; (benchmark 10 '(org-scheduler-next-tasks))
;; (benchmark 10 '(org-scheduler-headlines))

;; (benchmark 1 '(org-cache-create))
; 5.76
; 0.36

;; (org-cache-create)
                                        ; 1.06

;; (benchmark 10 '(org-scheduler-next-tasks))
;; (org-ql-search
;;   (org-scheduler-files)
;;   ;; (mapcar #'cadr (zk-files))
;;   '(property "TITLE"))

(provide 'org-scheduler)
