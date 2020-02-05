;; TODO: Group by source file
(defun org-zk-backlinks-for-file (file)
  "Files linking to FILE."
  (let ((links))
    (maphash
     (lambda (key value)
       (dolist (link (plist-get value :links))
         (if (string=
              file
              (plist-get link :full-path))
             (push (cons key link) links))))
     org-el-cache--table)
    links))

(defun org-zk-backlinks-for-buffer ()
  "Files linking to the current buffer"
  (org-zk-backlinks-for-file (buffer-file-name)))

(defcustom org-zk-backlink-buffer-position 'right
  "Position of the backlink buffer")

(defvar org-zk-backlink-buffer "*org-zk Backlinks*")

(defun org-zk-backlink-setup-buffer ()
  (display-buffer-in-side-window
   (get-buffer-create org-zk-backlink-buffer)
   `((side . ,org-zk-backlink-buffer-position))))

(defun org-zk-backlinks ()
  (interactive)
  (when (org-el-cache-member-p (buffer-file-name))
    (org-zk-backlink-setup-buffer)
    (org-zk-backlink-update-buffer)))

(defun org-zk-backlink-hook (&rest _args)
  (interactive)
  (when (eq major-mode 'org-mode)
    (if-let ((filename (buffer-file-name)))
        (if (org-el-cache-member-p filename)
            (org-zk-backlink-update-buffer)))))

(advice-add #'select-window :after #'org-zk-backlink-hook)

(defun org-zk-backlink-update-buffer ()
  (interactive)
  (let ((backlinks (org-zk-backlinks-for-buffer))
        (title (org-el-cache-file-keyword
                (buffer-file-name)
                "TITLE"
                (buffer-file-name))))
    (with-current-buffer org-zk-backlink-buffer
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (visual-line-mode)
      (insert (format "#+TITLE: Backlinks for %s\n\n" title))
      (dolist (backlink backlinks)
        (let ((source (car backlink)))
          (when-let ((entry (org-el-cache-get source)))
            (insert
             (format
              "* [[file:%s][%s]] \n"
              (car backlink)
              (org-el-cache-entry-keyword
               entry "TITLE"
               (car backlink))))
            (insert (plist-get (cdr backlink) :context) "\n")
            (insert "\n"))))
      (read-only-mode))))

(defun org-zk-trim-string (string)
  "Remove trailing newlines from STRING"
  (setq string (replace-regexp-in-string (rx (+ (any "\n"))) " " string))
  (replace-regexp-in-string (rx (+ (any "\n")) eol) "" string))

(defun org-zk-backlink-link-context (el)
  (if-let ((parent (org-element-property :parent el)))
      (cond
       ((eq (org-element-type parent) 'paragraph)
        (org-zk-trim-string (org-el-cache-interpret-data parent)))
       ((eq (org-element-type parent) 'headline)
        (org-zk-trim-string (org-element-property :raw-value parent)))
       (t (org-zk-backlink-link-context parent)))
    (org-zk-trim-string (org-el-cache-interpret-data el))))
