(defvar org-zk-titlecase-prepositions
  (list
 "in"
 "on"
 "at"
 "for"
 "ago"
 "before"
 "since"
 "to"
 "into"
  ;; "past" (only in "half past ten")
 "till"
 "until"
 "by"
  ;; "next to"
 "above"
 "across"
 "through"
 "towards"
 "onto"
 "from"
  ;; "out of"
 "about"
 "of"
  ;; "off" (get off the train)
  ))

(defvar org-zk-titlecase-articles (list "a" "an" "the"))

(defvar org-zk-titlecase-conjunctions (list
"and" "but" "or" "although" "because" "if" "since" "unless" "until" "while" "either" "or" "neither" "nor"))
;; missing: "not only" "but also"

(defun org-zk-titlecase--downcase-p (word)
  "Determine if WORD should be lowercase."
  (let ((word (downcase word)))
    (or
     (member word org-zk-titlecase-prepositions)
     (member word org-zk-titlecase-articles)
     (member word org-zk-titlecase-conjunctions))))

(defun org-zk-titlecase--fully-uppercase-p (word)
  "Check if a word contains only uppercase characters."
  (equal (upcase word) word))

(defun org-zk-titlecase--process-word (word)
  (cond
   ((org-zk-titlecase--fully-uppercase-p word) word)
   ((org-zk-titlecase--downcase-p word) (downcase word))
   (t (capitalize word))))

(defun org-zk-titlecase (input)
  "Convert a string to title-case."
  (let* ((words (split-string input))
         (first (pop words)))
    (if (null words)
        (if (org-zk-titlecase--fully-uppercase-p first)
            first
            (capitalize first))
      (concat (capitalize first)
              " "
              (mapconcat #'org-zk-titlecase--process-word words " ")))))

(provide 'org-zk-titlecase)
