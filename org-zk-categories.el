(defun org-zk-escape-filename (str)
  (setq str (replace-regexp-in-string " " "_" str))
  (setq str (replace-regexp-in-string
             (rx (not (in "a-zA-Z0-9" "-" "_"))) "" str))
  (setq str (downcase str))
  str)

(defun org-zk-default-link-fn (file title)
  (concat "[[zk_friend:" file "][" title "]]"))

(defun org-zk-default-name-fn (title)
  (org-zk-escape-filename title))

(defun org-zk-default-setup-fn (title)
  (insert "#+TITLE: " title "\n")
  (insert "#+CREATED: ")
  (org-insert-time-stamp (current-time) t t)
  (insert "\n\n"))

(defvar org-zk-file-blacklist '("./" "../" ".git/"))

(defun org-zk-category-name (p) (plist-get p :name))
(defun org-zk-category-path (p) (plist-get p :path))
(defun org-zk-category-ignore-p (p) (plist-get p :ignore))
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


(defun org-zk-category-for-file (filename)
  "Find the (sub)category FILENAME belongs to.
Returns NIL if FILENAME is not managed by org-zettelkasten."
  (seq-find
   (lambda (category)
     (string-prefix-p
      (expand-file-name (org-zk-category-path category))
      (expand-file-name filename)))
   org-zk-categories))

(defun org-zk-category-prompt (action)
  "Select a category by its name,
then call ACTION with the category that was selected."
  (interactive)
  (let ((collection
         (mapcar
          (lambda (c) (cons (plist-get c :name) c))
          org-zk-categories)))
    (ivy-read "Category: " collection :action action)))

(defun org-zk-category-prompt-or-current (action)
  (let ((category (org-zk-category-for-file (buffer-file-name))))
    (if category
        (funcall action category)
      (org-zk-category-prompt (lambda (cat) (funcall action (cdr cat)))))))

(def-org-el-cache-file-hook category (file _el)
  `(:category ,(org-zk-category-for-file file)))

(provide 'org-zk-categories)
