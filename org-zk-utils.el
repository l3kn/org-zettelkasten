(defun org-zk-skip-headers ()
  (while (looking-at-p "#+")
    (forward-line)))

(provide 'org-zk-utils)
