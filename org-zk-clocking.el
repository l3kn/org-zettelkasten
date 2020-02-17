;;; org-zk-clocking.el --- Cloking calculations for org-zettelkasten -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Package-requires: ((emacs "26.3"))
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Clock Class

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

;;; Cache Setup

(def-org-el-cache org-zk-clocking-cache
  (list org-zk-directory)
  (lambda (filename el)
    (list
     :clocks (org-element-map el 'clock #'org-zk-clock-from-element)))
  :include-archives t)

(org-el-cache-update org-zk-clocking-cache)

;;; Clocking Calculations

(defun org-zk-clocking-total (begin end)
  "Time total in the time from BEGIN to END."
  (ts-human-duration
   (org-el-cache-reduce
    org-zk-clocking-cache
    (lambda (filename entry acc)
      (+ (loop for clock in (plist-get entry :clocks) summing
               (org-zk-clock-duration-in-range clock begin end))
         acc))
    0)))

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
