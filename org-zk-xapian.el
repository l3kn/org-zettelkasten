(require 'org-zk-cache)

(defvar org-zk-xapian-db-filename "~/src/org-zettelkasten/xapian/org.db")
(defvar org-zk-xapian-script-filename "~/src/org-zettelkasten/xapian/org-xapian")

(defun org-zk-xapian--build-command (args)
  (concat
   org-zk-xapian-script-filename
   " "
   (mapconcat #'identity args " ")))

(defun org-zk-xapian-command (&rest args)
  (shell-command (org-zk-xapian--build-command args)))

(defun org-zk-xapian-command-to-string (&rest args)
  (shell-command-to-string (org-zk-xapian--build-command args)))

;; NOTE: Shell commands ending in ~&~ are executed asynchronously,
;; but that always outputs to a new buffer instead of the echo area.
(defun org-zk-xapian-refresh ()
  "Refresh the xapian index"
  (interactive)
  (with-temp-buffer
    (dolist (f (org-zk-cache-files))
      (insert f
              " "
              (org-zk-category-name (org-zk-category-for-file f))
              "\n"))
    (shell-command-on-region
     (point-min)
     (point-max)
     (org-zk-xapian--build-command `("index" ,org-zk-xapian-db-filename))
     (current-buffer))
    (message (string-trim (buffer-string)))))

(defun org-zk-xapian-refresh-file (category filename)
  "Updates a single file, expects an expanded file name"
  (org-zk-xapian-command "index-file" org-zk-xapian-db-filename filename category))

(defun org-zk-xapian-delete-file (filename)
  "Delete the entry for a file, expects an expanded file name"
  (org-zk-xapian-command "delete-entry" org-zk-xapian-db-filename filename))

(defun org-zk-xapian-rename-file (old-filename new-filename new-category-name)
  "Changes the filename of a database entry."
  (org-zk-xapian-command
   "rename-entry"
   org-zk-xapian-db-filename
   old-filename
   new-filename
   new-category-name))

(defun org-zk-xapian-query (query)
  "Execute a query on the Xapian database.
Returns a list of lists (file category title accuracy)"
  (mapcar
   (lambda (result)
     (let ((parts (split-string result "\t")))
       (list (first parts)
             (second parts)
             (third parts)
             (/ (string-to-number (fourth parts)) 100.0))))
   (split-string
    (org-zk-xapian-command-to-string "search" org-zk-xapian-db-filename query)
    "\n"
    t)))

(defun org-zk-xapian-delete-file-advice (filename &optional _trash)
  (let ((truename (file-truename filename)))
    (when (org-zk-cache-get truename)
      (message "Removing xapian file %s" truename)
      (org-zk-xapian-delete-file truename))))

(advice-add 'delete-file :before #'org-zk-xapian-delete-file-advice)

(defun org-zk-xapian-rename-file-advice (old-filename new-filename &optional _ok-if-already-exists)
  "1. make sure the new file is not a temporary file
2. make sure the old file was managed by xapian
3. if the new file is still managed by xapian, update the entry
   otherwise, delete it"
  (if (and
       (string= (file-name-extension new-filename) "org")
       (not (string-prefix-p (temporary-file-directory) new-filename)))
    (let* ((old-truename (file-truename old-filename))
           (new-truename (file-truename new-filename))
           (old-category (org-zk-category-for-file old-truename))
           (new-category (org-zk-category-for-file new-truename)))
      (if old-category
          (if new-category
              (progn
                (message
                 "Renaming xapian file %s to %s (%s)"
                 old-truename
                 new-truename
                 (org-zk-category-name new-category))
                (org-zk-xapian-rename-file
                 old-truename
                 new-truename
                 (org-zk-category-name new-category)))
            (progn
              (message "Removing moved xapian file %s" old-truename)
              (org-zk-xapian-delete-file old-truename)))))))

(advice-add 'rename-file :before #'org-zk-xapian-rename-file-advice)

(provide 'org-zk-xapian)
