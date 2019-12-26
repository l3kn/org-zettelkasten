(defun my-test ()
    (interactive)
  (if (looking-at-p "^$")
      (forward-line)
    (insert "nn\n")))

(defun org-zk-add-link (type target)
  "Add a link of TYPE to TARGET to the current files link list."
  ;; Skip keywords
  (save-excursion
    (goto-char (point-min))
    ;; skip keywords
    (while (looking-at-p "^#+")
      (forward-line))
      (let (p)
        (setq p (point))
        (if (looking-at-p "^$")
            (forward-line))
        ;; forward-line did not work / file empty besides keywords
        (if (= p (point))
            (insert "\n")))
      ;; TODO: Skip lines of "lesser / sorted prev" type
      (insert (format
               "- [[zk_%s:%s][%s]]\n"
               type
               target
               (org-zk-file-title target)))))

(defun org-zk-edge-inverse (type)
  (pcase type
    ("parent" "child")
    ("child" "parent")
    ("friend" "friend")))

(defun org-zk--add-edge (type target)
  (let ((inverse (org-zk-edge-inverse type))
        (source (buffer-file-name)))
  (org-zk-add-link type target)
    (when inverse
      (with-current-buffer (find-file-noselect target)
        (org-zk-add-link inverse source)))))

(defun org-zk-add-edge (type)
  "Add a new edge of TYPE. Prompts for an existing file.  If `ivy-immediate-return' is used,
creates a file with that title in category of the current file."
  (org-zk-select-file
   (lambda (selection)
     (org-zk--add-edge
      type
      (if (stringp selection)
          (org-zk-create-file
           selection
           (org-zk-category-for-file (buffer-file-name)))
        (cdr selection))))))

(defun org-zk-add-parent ()
  (interactive)
  (org-zk-add-edge "parent"))

(defun org-zk-add-child ()
  (interactive)
  (org-zk-add-edge "child"))

(defun org-zk-add-friend ()
  (interactive)
  (org-zk-add-edge "friend"))

(org-link-set-parameters
 "zk_parent"
 :complete #'org-file-complete-link
 :follow #'find-file)

(org-link-set-parameters
 "zk_child"
 :complete #'org-file-complete-link
 :follow #'find-file)

(org-link-set-parameters
 "zk_friend"
 :complete #'org-file-complete-link
 :follow #'find-file)

(defun org-zk-make-link (file &optional title)
  (let ((category (org-zk-category-for-file file)))
    (if category
        (funcall
         (org-zk-category-link-fn category)
         file
         (or title (org-zk-file-title file)))
      (error "File %s is not part of any zettelkasten category" file))))

(defun org-zk-files-linking-here ()
  "Generate a list of files linking to the current buffer."
  (let ((path (buffer-file-name)))
    (org-zk-cache-filter
     (lambda (source file)
       (--any (string= (oref it path) path)
              (oref file links))))))

(provide 'org-zk-links)
