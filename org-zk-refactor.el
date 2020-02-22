(defun org-zk-refactor-add-ids ()
  "Add IDs to all headlines of all files."
  (interactive)
  (dolist (file (org-zk-files))
    (org-zk-in-file file
      (org-zk-add-ids-to-headlines))))
