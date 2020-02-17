;; TODO: Group by source file
(defun org-zk-backlinks-for-file (file)
  "Files linking to FILE."
  (let ((links)
        (file (expand-file-name file)))
    (org-el-cache-each
     org-zk-cache
     (lambda (key value)
       (dolist (link (plist-get value :links))
         (if (string= file (plist-get link :full-path))
             (push (cons key link) links)))))
    links))

(defun org-zk-backlinks-for-buffer ()
  "Files linking to the current buffer"
  (org-zk-backlinks-for-file (buffer-file-name)))

(defcustom org-zk-backlink-buffer-position 'right
  "Position of the backlink buffer")

(defun org-zk-backlink-buffer () (get-buffer-create "*org-zk Backlinks*"))

(defun org-zk-backlink-setup-buffer ()
  (display-buffer-in-side-window
   (org-zk-backlink-buffer)
   `((side . ,org-zk-backlink-buffer-position))))

(defun org-zk-backlinks ()
  (interactive)
  (when (org-el-cache-member-p org-zk-cache (buffer-file-name))
    (org-zk-backlink-setup-buffer)
    (org-zk-backlink-update-buffer)))

(defun org-zk-backlink-hook (&rest _args)
  (interactive)
  (when (eq major-mode 'org-mode)
    (if-let ((filename (buffer-file-name)))
        (if (org-el-cache-member-p org-zk-cache filename)
            (org-zk-backlink-update-buffer)))))

(advice-add #'select-window :after #'org-zk-backlink-hook)

(defun org-zk-backlink-update-buffer ()
  (interactive)
  (let ((backlinks (org-zk-backlinks-for-buffer))
        (title (or
                (plist-get (org-el-cache-get
                            org-zk-cache
                            (buffer-file-name))
                           :title)
                (buffer-file-name))))
    (with-current-buffer (org-zk-backlink-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (visual-line-mode)
      (insert (format "#+TITLE: Backlinks for %s\n\n" title))
      (dolist (backlink backlinks)
        (let ((source (car backlink)))
          (when-let ((entry (org-el-cache-get org-zk-cache source)))
            (insert
             (format
              "* [[file:%s][%s]] \n"
              (car backlink)
              (or (plist-get entry :title) (car backlink))))
            (insert (plist-get (cdr backlink) :context) "\n")
            (insert "\n"))))
      (read-only-mode))))

(provide 'org-zk-backlinks)
