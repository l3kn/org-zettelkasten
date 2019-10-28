(defvar zk-xapian-db-filename "~/src/zettelkasten-xapian/org.db")
(defvar zk-xapian-script-filename "~/src/zettelkasten-xapian/org-xapian")

(defun zk-xapian--build-command (args)
  (concat
   zk-xapian-script-filename
   " "
   (mapconcat #'identity args " ")))

(defun zk-xapian-command (&rest args)
  (shell-command (zk-xapian--build-command args)))

(defun zk-xapian-command-to-string (&rest args)
  (shell-command-to-string (zk-xapian--build-command args)))

;; NOTE: Shell commands ending in ~&~ are executed asynchronously,
;; but that always outputs to a new buffer instead of the echo area.
(defun zk-xapian-refresh ()
  "Refresh the xapian index"
  (interactive)
  (with-temp-buffer
    (dolist (f (zk-files))
      (insert (car f) " " (cadr f) "\n"))
    (shell-command-on-region
     (point-min)
     (point-max)
     (zk-xapian--build-command `("index" ,zk-xapian-db-filename))
     (current-buffer))
    (message (string-trim (buffer-string)))))

(defun zk-xapian-refresh-file (project filename)
  "Updates a single file, expects an expanded file name"
  (zk-xapian-command "index-file" zk-xapian-db-filename filename project))

(defun zk-xapian-delete-file (filename)
  "Delete the entry for a file, expects an expanded file name"
  (zk-xapian-command "delete-entry" zk-xapian-db-filename filename))

(defun zk-xapian-rename-file (old-filename new-filename new-project-name)
  "Changes the filename of a database entry."
  (zk-xapian-command
   "rename-entry"
   zk-xapian-db-filename
   old-filename
   new-filename
   new-project-name))

(defun zk-xapian-query (query)
  "Execute a query on the Xapian database.
Returns a list of lists (file project title accuracy)"
  (mapcar
   (lambda (result)
     (let ((parts (split-string result "\t")))
       (list (first parts)
             (second parts)
             (third parts)
             (/ (string-to-number (fourth parts)) 100.0))))
   (split-string
    (zk-xapian-command-to-string "search" zk-xapian-db-filename query)
    "\n"
    t)))

(defun delete-file-zk-xapian-delete-file (filename &optional _trash)
  (let ((truename (file-truename filename)))
    (when (zk-file-p truename)
      (message "Removing xapian file %s" truename)
      (zk-xapian-delete-file truename))))

(advice-add 'delete-file :before #'delete-file-zk-xapian-delete-file)

(defun rename-file-zk-xapian-rename-file (old-filename new-filename &optional _ok-if-already-exists)
  "1. make sure the new file is not a temporary file
2. make sure the old file was managed by xapian
3. if the new file is still managed by xapian, update the entry
   otherwise, delete it"
  (unless (string-prefix-p (temporary-file-directory) new-filename)
    (let* ((old-truename (file-truename old-filename))
           (new-truename (file-truename new-filename))
           (old-project (zk-file-project old-truename))
           (new-project (zk-file-project new-truename)))
      (if old-project
          (if new-project
              (progn
                (message
                 "Renaming xapian file %s to %s (%s)"
                 old-truename
                 new-truename
                 (zk-project-name new-project))
                (zk-xapian-rename-file
                 old-truename
                 new-truename
                 (zk-project-name new-project)))
            (progn
              (message "Removing moved xapian file %s" old-truename)
              (zk-xapian-delete-file old-truename)))))))

(advice-add 'rename-file :before #'rename-file-zk-xapian-rename-file)

(provide 'org-xapian)
