(require 'org-zk-titlecase)

(defvar org-zk-edges-block-name "ZK_EDGES")
(defvar org-zk-edges-block-begin (concat "#+BEGIN_" org-zk-edges-block-name))
(defvar org-zk-edges-block-end (concat "#+END_" org-zk-edges-block-name))

(defun org-zk-edges-insert-block ()
  (interactive)
  (let* ((options (org-buffer-options))
         (end (oref options end)))
    (save-excursion
      (goto-char end)
      (insert "\n")
      (insert "#+BEGIN_" org-zk-edges-block-name "\n")
      (setq marker (point-marker))
      (insert "#+END_" org-zk-edges-block-name "\n")
      (org-zk-edges
       :edges nil
       :beginning marker
       :end marker))))

(defclass org-zk-edge ()
  ((key :type string :initarg :key)
   (value :type string :initarg :value)
   (beginning :type marker :initarg :beginning)
   (end :type marker :initarg :end)))

(defclass org-zk-edges ()
  ((edges :type list :initarg :edges)
   (beginning :type marker :initarg :beginning)
   (end :type marker :initarg :end)))

(defvar org-zk-edge-re
      (rx bol "- " (group (+ (or alpha "_" "-"))) " :: " (group (+ nonl)) eol))

(defun org-zk-buffer-edges ()
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((beginning (search-forward org-zk-edges-block-begin nil t)))
        (when beginning
          (beginning-of-line)
          (forward-line)
          (let (edges (end-marker (make-marker)))
            (while (looking-at org-zk-edge-re)
              (let ((marker-beg (make-marker))
                    (marker-end (make-marker)))
                (set-marker marker-beg (match-beginning 2))
                (set-marker marker-end (match-end 2))
                (push
                 (cons
                  (intern (match-string-no-properties 1))
                  (org-zk-edge
                   :key (match-string-no-properties 1)
                   :value (match-string-no-properties 2)
                   :beginning marker-beg
                   :end marker-end))
                 edges)
                (forward-line)
                (setq end-marker (point-marker))))
            (org-zk-edges
             :beginning (let ((m (mark-marker)))
                          (set-marker m beginning)
                          m)
             :end end-marker
             :edges edges)
            ))))))

(cl-defmethod add-edge ((edges org-zk-edges) key value)
  (save-excursion
    (goto-char (oref edges end))
    (insert (format "- %s :: %s\n" key value))))

(cl-defmethod org-zk-sort-edges ((edges org-zk-edges))
    (sort-lines
     nil
     (oref edges beginning)
     (oref edges end)))

(setq org-zk-edge-types
      (list "Parent" "Friend" "Author" "Book" "Child" "Member of" "Member"))

(setq org-zk-edge-pairs
      '(("Parent" . "Child")
        ("Book" . "Author")
        ("Replaces" . "Replaced By")
        ("Client For" . "Server For")
        ("Member Of" . "Has Member")
        ("Friend" . "Friend")))

(defun org-zk-edge-inverse (type)
  (or (cdr (assoc type org-zk-edge-pairs))
      (car (rassoc type org-zk-edge-pairs))))

(defun org-zk--add-edge-to-buffer (type target)
  (let ((edges (or (org-zk-buffer-edges)
                   (org-zk-edges-insert-block)))
        (source (buffer-file-name)))
    (add-edge edges type (org-zk-make-link target))
    (org-zk-sort-edges edges)
    (save-buffer)))

(defun org-zk--add-edge (type target)
    (org-zk--add-edge-to-buffer type target)
    (let ((inverse (org-zk-edge-inverse type))
          (source (buffer-file-name)))
      (when inverse
        (with-current-buffer (find-file-noselect target)
          (org-zk--add-edge-to-buffer inverse source)))))

(defun org-zk-file-edges (path)
  (with-current-buffer (find-file-noselect path)
    (or (org-zk-buffer-edges) (org-zk-edges-insert-block))))

(defun org-zk-add-edge (type)
  (interactive (list (ivy-completing-read "Type: " org-zk-edge-types)))
  (org-zk-select-file
   (lambda (selection)
     (org-zk--add-edge type (cdr selection)))))

(defun org-zk-create-edge (edge-type title)
  (interactive (list
                (ido-completing-read "Type: " org-zk-edge-types)
                (org-zk-titlecase (string-trim (read-string "Title: ")))))
  (org-zk-category-prompt-or-current
   (lambda (category)
     (let* ((link-fn (org-zk-category-link-fn category))
            (name-fn (org-zk-category-name-fn category))
            (setup-fn (org-zk-category-setup-fn category))
            (name (funcall name-fn title))
            (file (expand-file-name
                     (concat name ".org")
                     (org-zk-category-path category))))
         (if (file-exists-p file)
             (error "Aborting, file already exists: %s" file))
         (with-current-buffer (find-file-noselect file)
           (funcall setup-fn title)
           (save-buffer))
         (org-zk--add-edge edge-type file)
         (find-file file)))))

(defun org-zk-add-parent ()
  (interactive)
  (org-zk-add-edge "Parent"))

(defun org-zk-add-child ()
  (interactive)
  (org-zk-add-edge "Child"))

(defun org-zk-add-friend ()
  (interactive)
  (org-zk-add-edge "Friend"))

(defun org-zk-create-parent (title)
  (interactive (list (org-zk-titlecase (string-trim (read-string "Title: ")))))
  (org-zk-create-edge "Parent" title))

(defun org-zk-create-child (title)
  (interactive (list (org-zk-titlecase (string-trim (read-string "Title: ")))))
  (org-zk-create-edge "Child" title))

(defun org-zk-create-friend (title)
  (interactive (list (org-zk-titlecase (string-trim (read-string "Title: ")))))
  (org-zk-create-edge "Friend" title))

(defun org-zk-file-title (file)
  (let ((cache-file (org-cache-get file)))
    (if cache-file
        (org-cache-get-keyword cache-file "TITLE")
      filename)))

(defun org-zk-make-link (file &optional title)
  (let ((category (org-zk-category-for-file file)))
    (if category
        (funcall
         (org-zk-category-link-fn category)
         file
         (or title (org-zk-file-title file)))
      (error "File %s is not part of any zettelkasten category" file))))

(provide 'org-edges)
