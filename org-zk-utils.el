(defun org-zk-skip-headers ()
  (while (looking-at-p "#+")
    (forward-line)))

(defun org-zk-read-title ()
  "Read a string to use as document title."
  (-> (read-string "Title: ") string-trim org-zk-titlecase))

(provide 'org-zk-utils)
