(defvar quickselect-keys '(?n ?e ?r ?a ?t ?i ?d ?u))

(defun org-quickselect-effort-prompt (cur allowed)
  (message "[RET%s]: %s"
           (if cur (concat "=" cur) "")
           (mapconcat 'identity
                      (loop for estimate in (cons "custom" allowed)
                            for character in quickselect-keys
                            collect (format "%s(%c)" estimate character))
                      " "))
  (let ((rpl (read-char-exclusive)))
    (if (equal rpl ?\r)
        cur
      (let ((pos (position rpl quickselect-keys)))
        (if (and pos (/= pos 0) (<= pos (length allowed)))
            (nth (1- pos) allowed)
          (org-completing-read "Effort: " allowed nil))))))

(defun org-quickselect-effort ()
  (interactive)
  (let* ((completion-ignore-case t)
         (prop org-effort-property)
         (cur (org-entry-get nil prop))
         (allowed (org-property-get-allowed-values nil org-effort-property))
         (current (mapcar 'list (org-property-values org-effort-property)))
         (heading (nth 4 (org-heading-components)))
         (val (if allowed
                  (org-quickselect-effort-prompt cur allowed)
                (org-completing-read
                 (concat "Effort" (and cur (string-match "\\S-" cur)
                                       (concat " [" cur "]"))
                         ": ")
                 current nil nil "" nil cur))))
    (org-set-effort nil val)))

(provide 'org-quickselect-effort)
