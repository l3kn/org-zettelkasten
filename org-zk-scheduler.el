(use-package org-ql)
(require 'org-zk-cache)

(defun org-zk-scheduler-files ()
  (mapcar
   #'car
   (org-zk-cache-file-query '(keyword "GTD_STATE" "active"))))

(defun org-zk-scheduler-headlines ()
  (mapcar
   (lambda (headline) (oref headline title))
   (org-zk-cache-headline-query
    '(keyword "GTD_STATE" "active")
    '(or (todo "NEXT")))))

(defun org-zk-scheduler-next-tasks ()
  (org-ql-select
    (org-zk-scheduler-files)
    '(todo "NEXT")))

(defun random-list-element (list)
  (nth (random (length list)) list))

(defun org-zk-scheduler-random-next-task ()
  (random-list-element (org-zk-scheduler-next-tasks)))

(defun org-zk-scheduler-next-tasks ()
  (interactive)
  (org-ql-select
    (org-zk-scheduler-files)
    '(or (todo "NEXT"))))

;; (length (org-zk-scheduler-next-tasks))
;; (length (org-zk-scheduler-headlines))
;; 324 NEXT
;; ...
;; 173 NEXT

;; (benchmark 10 '(org-zk-scheduler-next-tasks))
;; (benchmark 10 '(org-zk-scheduler-headlines))

;; (benchmark 1 '(org-zk-cache-create))
; 5.76
; 0.36

;; (org-zk-cache-create)
                                        ; 1.06
;; (benchmark 10 '(org-zk-scheduler-next-tasks))

(provide 'org-zk-scheduler)
