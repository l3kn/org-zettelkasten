(require 'rx)

(defvar org-zk-keywords-re
      (rx bol "#" "+" (group (+ (or alpha "_" "-"))) ":" (* blank) (group (+ nonl)) eol))

(defun org-zk-keywords-set-or-add (key value)
  "Sets the value of keyword KEY if it already exists,
if not, adds an entry at the end of the files keywords."
  (let (found)
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (looking-at org-zk-keywords-re)
          (if (string-equal (match-string 1) key)
              (progn
                (goto-char (match-beginning 2))
                (kill-line)
                (insert value)
                (setq found t))
            (forward-line)))
        (unless found
          (insert (format "#+%s: %s\n" key value)))))))

(defun org-zk-keywords-add (key value)
  "Adds a keyword #+KEY: VALUE at the end of the files keywords."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (looking-at org-zk-keywords-re)
        (forward-line))
      (insert (format "#+%s: %s\n" key value)))))

(defun org-zk-keywords-delete (key)
  "Deletes all keywords with KEY.
Returns T if at least one keyword was deleted, NIL if not."
  (let (found)
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (looking-at org-zk-keywords-re)
          (if (string-equal (match-string 1) key)
              (progn (kill-line)
                     (setq found t))
            (forward-line)))
        found))))


(defmacro org-zk-def-keyword (key values)
  "Macro for registering keyword-setting functions"
  (let ((name
         (->>
          (downcase key)
          (replace-regexp-in-string "_" "-")
          (format "org-zk-set-%s")
          intern)))
    `(defun ,name (value)
       (interactive
        (list (ivy-completing-read
          (format "%s: " ,key)
          ,values)))
       (org-zk-keywords-set-or-add ,key value))))

(org-zk-def-keyword
 "GTD_STATE"
 '("active"
   "someday"
   "planning"
   "cancelled"
   "done"))

(org-zk-def-keyword
 "STABILITY"
 '("stable"
   "wip"))

(org-zk-def-keyword
 "CATEGORY"
 '("book"
   "person"
   "concept"
   "location"))

(org-zk-def-keyword
 "FC_STATE"
 '("suspended"
   "active"))

(provide 'org-zk-keywords)
