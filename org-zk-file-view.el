(require 'org-zk-query)

;;; Font Faces

(defface org-zk-file-view-date-face
  '((((class color) (background light)) (:foreground "#aaa"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used for dates in the file view."
  :group 'org-zk)

(defface org-zk-file-view-title-face
  '((((class color) (background light)) (:foreground "#000" :weight bold))
    (((class color) (background dark))  (:foreground "#fff" :weight bold)))
  "Face used for titles in the file view."
  :group 'org-zk)

(defface org-zk-file-view-keyword-face
  '((((class color) (background light)) (:foreground "#070"))
    (((class color) (background dark))  (:foreground "#0f0")))
  "Face used for keywords in the file view."
  :group 'org-zk)

(defface org-zk-file-view-collection-face
  '((((class color) (background light)) (:foreground "#aa0"))
    (((class color) (background dark))  (:foreground "#ff0")))
  "Face used for org-zk collections in the file view."
  :group 'org-zk)

;;; Tabulated List Mode

(setq org-zk-file-view-format
      (vector
       '("Collection" 8 t)
       '("Created" 10 t)
       '("Title" 50 t)))

(defun org-zk-cache-get-keyword (entry kw &optional default)
  (alist-get
   kw
   (plist-get entry :keywords)
   default
   nil
   #'string=))

(defun org-zk-entry-gtd-state (entry)
  (alist-get
   "GTD_STATE"
   (plist-get entry :org-keywords)
   org-zk-default-file-state nil 'string=))

;; TODO: This doesn't do anything besides checking for todos
(defun org-zk-project-p (filename entry)
  (let* ((keywords (plist-get entry :org-keywords))
         (state (alist-get "GTD_STATE" keywords nil nil 'string=)))
    (org-zk--has-todos entry)))

(defun org-zk-file-view-tabulate-title (file)
  (if (null (plist-get file :keywords))
      (propertize
       (plist-get file :title)
       'face 'org-zk-file-view-title-face)
    (concat
     (propertize
      (plist-get file :title)
      'face 'org-zk-file-view-title-face)
     " ("
     (mapconcat
      (lambda (kw) (propertize kw 'face 'org-zk-file-view-keyword-face))
      (plist-get file :keywords) ",")
     ")")))

(defun org-zk-file-view-tabulate (files)
  (mapcar
   (lambda (file)
     (list
      file
      (vector
       (propertize
        (plist-get (plist-get file :collection) :name)
        'face 'org-zk-file-view-collection-face)
       (propertize
        (if-let ((created (plist-get file :created))) (subseq created 1 11) "")
        'face 'org-zk-file-view-date-face)
       (org-zk-file-view-tabulate-title file))))
   files))

(defun org-zk-file-view-buffer ()
  (get-buffer-create "org-zk File View"))

(defun org-zk-file-view-show (query)
  (setq org-zk-file-view-filter query)
  (let* ((pred (org-zk-query query org-zk-query-file-predicates))
         (files (org-el-cache-select org-zk-cache pred)))
    (with-current-buffer (org-zk-file-view-buffer)
      (org-zk-file-view-mode)
      (setq tabulated-list-format org-zk-file-view-format)
      (tabulated-list-init-header)
      (setq tabulated-list-entries (org-zk-file-view-tabulate files))
      (setq tabulated-list-sort-key nil)
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(define-derived-mode org-zk-file-view-mode tabulated-list-mode "org-zk Files"
  "Major mode for listing org-zk files"
  (hl-line-mode))

(setq org-zk-file-view-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map tabulated-list-mode-map)
        (define-key map (kbd "RET") 'org-zk-file-view-open)
        (define-key map (kbd "S") 'org-zk-file-view)
        (define-key map (kbd "s") 'org-zk-file-view)
        (define-key map (kbd "k") 'org-zk-file-view-add-keyword)
        (define-key map (kbd "K") 'org-zk-file-view-edit-keywords)
        map))

(defun org-zk-file-view-open ()
  (interactive)
  (let ((file (tabulated-list-get-id)))
    (find-file (plist-get file :file))))

(defun org-zk-file-view-edit-keywords ()
  "Edit the keywords of the current entry"
  (interactive)
  (let* ((entry (tabulated-list-get-id))
         (kws (split-string
               (read-string
                "Keywords: "
                (mapconcat #'identity (plist-get entry :keywords) " "))
               " " t)))
    (plist-put entry :keywords kws)
    (tabulated-list-set-col "Title" (org-zk-file-view-tabulate-title entry))
    (org-zk-set-file-keywords (plist-get entry :file) kws)))

(defun org-zk-file-view-add-keyword ()
  "Add a keyword to the current entry"
  (interactive)
  (let* ((entry (tabulated-list-get-id))
         (kw (org-zk-read-keyword))
         (kws (org-zk-sort-strings
               (remove-duplicates
                (cons kw (plist-get entry :keywords))
                :test #'string=))))
    (plist-put entry :keywords kws)
    (tabulated-list-set-col "Title" (org-zk-file-view-tabulate-title entry))
    (org-zk-set-file-keywords
     (plist-get entry :file) kws)))

(defun org-zk-set-file-keywords (file kws)
  (org-zk-in-file file
    (org-zk-keywords-set-or-add
     "KEYWORDS"
     (mapconcat #'identity kws " "))))

(defvar org-zk-file-view-filter ""
  "Current filter of the file view")

(defun org-zk-file-view (query)
  (interactive (list (read-string "Filter: " org-zk-file-view-filter)))
  (org-zk-file-view-show query))

(provide 'org-zk-file-view)
