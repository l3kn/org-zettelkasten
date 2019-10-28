(defun org-zk-escape-filename (str)
  (setq str (replace-regexp-in-string " " "_" str))
  (setq str (replace-regexp-in-string (rx (not (in "A-Z0-9" "-" "_"))) "" str))
  (setq str (downcase str))
  str)

(defun org-zk-default-link-fn (file title)
  (concat "[[file:" file "][" title "]]"))

(defun org-zk-default-name-fn (title)
  (org-zk-escape-filename title))

(defun org-zk-default-setup-fn (title)
  (insert "#+TITLE: " title "\n")
  (insert "#+CREATED: ")
  (org-insert-time-stamp (current-time) t t)
  (insert "\n")
  (insert "#+UPDATED: ")
  (org-insert-time-stamp (current-time) t t)
  (insert "\n\n"))

(defvar org-zk-file-blacklist '("./" "../" ".git/"))


(defun org-zk-category-name (p) (plist-get p :name))
(defun org-zk-category-path (p) (plist-get p :path))
(defun org-zk-category-recursive-p (p) (plist-get p :recursive))
(defun org-zk-category-subcategories (p) (plist-get p :subcategories))
(defun org-zk-category-link-fn (p)
  (or
   (plist-get p :link-fn)
   'org-zk-default-link-fn))
(defun org-zk-category-name-fn (p)
  (or
   (plist-get p :name-fn)
   'org-zk-default-name-fn))
(defun org-zk-category-setup-fn (p)
  (or
   (plist-get p :setup-fn)
   'org-zk-default-setup-fn))

(defun org-zk--subcategory-for-path (category path)
  "Given a category and the path for a folder in its path,
return the subcategory for this path, if it exists."
  (seq-find
   (lambda (subcategory)
     (string= (org-zk-category-path subcategory) path))
   (org-zk-category-subcategories category)))

(defun org-zk-category-files-recursively (category path)
  (mapcar
   (lambda (f) (cons f category))
   (directory-files-recursively path ".org$" nil)))

;; Based on ~directory-files-recursively~
(cl-defun org-zk-category-files (category &optional (parent-path ""))
  "Generate a list of '(filename project) entries for all files
of CATEGORY."
  (let ((result nil)
        (files nil)
        (dir (expand-file-name (org-zk-category-path category) parent-path)))
    (dolist (file (file-name-all-completions "" dir))
      (unless (member file org-zk-file-blacklist)
        (cond ((directory-name-p file)
               (let* ((subcategory (org-zk--subcategory-for-path category file))
                      (leaf (substring file 0 (1- (length file))))
                      (full-file (expand-file-name leaf dir)))
                 ;; Don't follow symlinks to other directories.
                 (unless (file-symlink-p full-file)
                   (cond
                    (subcategory
                     (setq result
                           (nconc result
                                  (org-zk-category-files subcategory dir))))
                    ((org-zk-category-recursive-p category)
                     (setq result
                           (nconc result
                                  (org-zk-category-files-recursively category full-file))))))))
              ((string= (file-name-extension file) "org")
               (push (cons (expand-file-name file dir) category)
                     files)))))
    (nconc result files)))

(defun org-zk-files-with-categories ()
  "Generate a list of '(filename project) entries for all files
managed by org-zettelkasten."
  (mapcan
   #'org-zk-category-files
   org-zk-categories))

(defun org-zk--category-for-file (filename categories parent-path)
  (let ((category
         (seq-find
          (lambda (category)
            (string-prefix-p
             (expand-file-name (org-zk-category-path category) parent-path)
             filename))
          categories)))
    (if category
        (or
         (org-zk--category-for-file
          filename
          (org-zk-category-subcategories category)
          (expand-file-name (org-zk-category-path category) parent-path))
         category))))

(defun org-zk-category-for-file (filename)
  "Find the (sub)category FILENAME belongs to.
Returns NIL if FILENAME is not managed by org-zettelkasten."
  (org-zk--category-for-file
   filename
   org-zk-categories
   ""))

(defun org-zk--flatten-category (category)
  "Flattens CATEGORY into a list of categories,
expanding the paths of its subcategories."
  (let ((subcategories
         (mapcan
          #'org-zk--flatten-category
          (plist-get category :subcategories))))
    (cons
     category
     (mapcar
      (lambda (subcategory)
        (plist-put subcategory
                   :path
                   (expand-file-name
                    (plist-get subcategory :path)
                    (plist-get category :path))))
      subcategories))))

(defun org-zk-categories-flat ()
  "Return a flattened list of categories,
each with a path expanded to include its parents paths."
  (mapcan
   #'org-zk--flatten-category
   org-zk-categories))

(defun org-zk-category-prompt (action)
  "Select a category by its name,
then call ACTION with the category that was selected."
  (interactive)
  (let* ((categories (org-zk-categories-flat))
         (collection
          (mapcar
           (lambda (c) (cons (plist-get c :name) c))
           categories)))
    (ivy-read "Category: " collection :action action)))

(defun org-zk-category-prompt-or-current (action)
  (let ((category (org-zk-category-for-file (buffer-file-name))))
    (if category
        (funcall action category)
      (org-zk-category-prompt action))))

(provide 'org-zk-categories)
