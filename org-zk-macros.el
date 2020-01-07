(defmacro org-zk-in-file (path &rest body)
  "If there is a buffer visiting PATH, use it to evaluate BODY,
If not, open PATH in the background, evaluate BODY, then save it."
  (declare (indent defun))
  `(let ((buffer (find-buffer-visiting ,path)))
     (if buffer
         (with-current-buffer buffer
           ,@body
           (save-buffer))
       (with-current-buffer (find-file-noselect ,path)
         ,@body
         (save-buffer)
         (kill-buffer)))))

(provide 'org-zk-macros)
