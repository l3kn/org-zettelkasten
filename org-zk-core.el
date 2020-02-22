;;; Configuration

(defvar org-zk-gtd-states
  '("active"
    "someday"
    "planning"
    "cancelled"
    "done"))

(defvar org-zk-default-file-priority "B")
(defvar org-zk-default-file-priorities '("A" "B" "C"))
(defvar org-zk-default-file-state "active")

(defvar org-zk-gtd-state-keyword "GTD_STATE")

(defcustom org-zk-directory "~/org"
  "Directory to treat as a Zettelkasten."
  :type 'string
  :group 'org-zk)

;;; Helper Functions / Macros

(defun org-zk-trim-string (string)
  "Remove trailing newlines from STRING"
  (setq string (replace-regexp-in-string (rx (+ (any "\n"))) " " string))
  (replace-regexp-in-string (rx (+ (any "\n")) eol) "" string))

(defmacro org-zk-in-file (path &rest body)
  "If there is a buffer visiting PATH, use it to evaluate BODY,
If not, open PATH in the background, evaluate BODY, then save it.
The result of the last expression in BODY is returned."
  (declare (indent defun))
  `(let ((buffer (find-buffer-visiting ,path)))
     (if buffer
         (with-current-buffer buffer
           (prog1 (progn ,@body)
             (org-el-cache-process-buffer)))
       (with-current-buffer (find-file-noselect ,path)
         (org-mode)
         (prog1 (progn ,@body)
           (save-buffer)
           (kill-buffer))))))

(defun org-zk--skip-keywords ()
  "Move to the beginning of a files content."
  (goto-char (point-min))
  (while (looking-at-p "^#+")
    (forward-line)))

(defun org-zk--beginning-of-main-content (file)
  "Find point at the beginning of the main content of FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (org-zk--skip-keywords)
      (point))))


;;; Collection Setup

(defun org-zk-escape-filename (str)
  (setq str (replace-regexp-in-string "ä" "ae" str))
  (setq str (replace-regexp-in-string "ö" "oe" str))
  (setq str (replace-regexp-in-string "ü" "ue" str))
  (setq str (replace-regexp-in-string " " "_" str))
  (setq str (replace-regexp-in-string
             (rx (not (in "a-zA-Z0-9" "-" "_"))) "" str))
  (setq str (downcase str))
  str)

(defun org-zk-default-name-fn (title)
  (org-zk-escape-filename title))

(defun org-zk-default-setup-fn (title)
  (insert "#+TITLE: " title "\n")
  (insert "#+CREATED: ")
  (org-insert-time-stamp (current-time) t t)
  (insert "\n\n"))

(defvar org-zk-file-blacklist '("./" "../" ".git/"))

(defun org-zk-collection-ignore-p (p) (plist-get p :ignore))
(defun org-zk-collection-name-fn (p)
  (getf p :name-fn 'org-zk-default-name-fn))
(defun org-zk-collection-setup-fn (p)
  (getf p :setup-fn 'org-zk-default-setup-fn))

(defun org-zk-collection-for-file (filename)
  "Find the (sub)collection FILENAME belongs to.
Returns NIL if FILENAME is not managed by org-zettelkasten."
  (seq-find
   (lambda (c)
     (string-prefix-p
      (expand-file-name (plist-get c :path))
      (expand-file-name filename)))
   org-zk-collections))

(defun org-zk-select-collection (action)
  "Select a collection by its name,
then call ACTION with the collection that was selected."
  (interactive)
  (let ((collection
         (mapcar
          (lambda (c) (cons (plist-get c :name) c))
          org-zk-collections)))
    (ivy-read "Collection: " collection :action action)))

;;; Cache Selector Functions

(defun org-zk-link-context (el)
  (if-let ((parent (org-element-property :parent el)))
      (cond
       ((eq (org-element-type parent) 'paragraph)
        (org-zk-trim-string (org-el-cache-interpret-data parent)))
       ((eq (org-element-type parent) 'headline)
        (org-zk-trim-string (org-element-property :raw-value parent)))
       (t (org-zk-link-context parent)))
    (org-zk-trim-string (org-el-cache-interpret-data el))))

(defun org-zk--cache-links (el)
  (org-element-map el 'link
    (lambda (child)
      (list
       :type (org-element-property :type child)
       :path (org-element-property :path child)
       :full-path (expand-file-name (org-element-property :path child))
       :text (org-el-cache-interpret-data (org-element-contents child))
       :context (org-zk-link-context child)))))

(defun org-zk--cache-keywords (el)
  (let ((first (car (org-element-contents el))))
    (if (eq (org-element-type first) 'section)
        (org-element-map first 'keyword
          (lambda (e) (cons
                  (org-element-property :key e)
                  (org-element-property :value e)))))))

(defun org-zk--cache-headlines (el)
  (org-element-map el 'headline
    (lambda (child)
      (list
       :begin (org-element-property :begin child)
       :deadline (org-element-property :deadline child)
       :scheduled (org-element-property :scheduled child)
       :level (org-element-property :level child)
       :priority (org-element-property :priority child)
       :tags (org-element-property :tags child)
       :todo-keyword (org-element-property :todo-keyword child)
       :todo-type (org-element-property :todo-type child)
       :title (org-element-property :raw-value child)
       :id (org-element-property :ID child)
       :effort (org-element-property :EFFORT child)
       :style (org-element-property :STYLE child)))))

;;; Cache Setup

(def-org-el-cache org-zk-cache
  (list org-zk-directory)
  (lambda (filename el)
    (let ((org-keywords (org-zk--cache-keywords el))
          (collection (org-zk-collection-for-file filename)))
      (list
       :file filename
       :links (org-zk--cache-links el)
       :org-keywords org-keywords
       :keywords
       (split-string (alist-get "KEYWORDS" org-keywords "" nil #'string=) " " t)
       :created
       (alist-get "CREATED" org-keywords nil nil #'string=)
       :title
       (or (alist-get "TITLE" org-keywords nil nil #'string=)
           (file-relative-name filename (plist-get collection :path)))
       :collection (org-zk-collection-for-file filename)
       :headlines (org-zk--cache-headlines el)))))

;;;; File keywords

(defun org-zk-file-title (file)
  "If FILE is in the org cache, return its title,
if not, return its filename."
  (let ((file (expand-file-name file)))
    (if-let ((entry (org-el-cache-get org-zk-cache file)))
        (plist-get entry :title)
      file)))

;;;; Cache Initialization

(defun org-zk-cache-update ()
  "Update the zettelkasten cache."
  (interactive)
  (org-el-cache-update org-zk-cache)
  (if (boundp 'org-zk-clocking-cache)
    (org-el-cache-update org-zk-clocking-cache)))

(defun org-zk-cache-force-update ()
  "Force-update / recreate the zettelkasten cache."
  (interactive)
  (org-el-cache-clear org-zk-cache)
  (org-el-cache-update org-zk-cache)
  (when (boundp 'org-zk-clocking-cache)
    (org-el-cache-clear org-zk-clocking-cache)
    (org-el-cache-update org-zk-clocking-cache)))

;;;; Cache Update Hooks

(if (fboundp 'ace-window)
    (defadvice ace-window (before other-frame-now activate)
      (when buffer-file-name (org-el-cache-process-buffer))))

;;; File Selection

(defun org-zk-files-with-titles ()
  "Returns an alist of entries (title-with-collection . filename)"
  (org-el-cache-map
   org-zk-cache
   (lambda (filename entry)
     (let ((collection (plist-get entry :collection))
           (title (plist-get entry :title)))
       (if collection
           (cons
            (format "%s (%s)" title (plist-get collection :name))
            filename)
         (cons (format "%s" title) filename))))))

(defvar org-zk-ivy-histoy nil)

(defun org-zk-select-file (action)
  (ivy-read
   "File: "
   (org-zk-files-with-titles)
   :history 'org-zk-ivy-history
   :action action))

(defun org-zk-open-file ()
  "Select a file, then open it"
  (interactive)
  (org-zk-select-file
   (lambda (selection)
     (if (stringp selection)
         (org-zk-new-file selection)
         (find-file (cdr selection))))))

;;; Linking Files

(defun org-zk-make-link (target &optional title)
  (concat
   "[[file:" target "][" (or title (org-zk-file-title target)) "]]"))

(defun org-zk-link-file ()
  "Select a file, then insert an org-mode link to it,
If `ivy-immediate-return' is used,
creates a file with that title in collection of the current file."
  (interactive)
  (org-zk-select-file
   (lambda (selection)
     (unless (or (bolp) (looking-at " "))
       (insert " "))
     (if (stringp selection)
         (let* ((title (org-zk-titlecase (string-trim selection)))
                (target (org-zk-create-file
                         title
                         (org-zk-collection-for-file (buffer-file-name)))))
           (insert (org-zk-make-link target title)))
       (insert (org-zk-make-link
                (file-relative-name
                 (cdr selection)
                 (file-name-directory (buffer-file-name)))
                (org-zk-file-title (cdr selection))))))))

;;;; Finding Linked Files

(defun org-zk-files-linking-to (file)
  "Generate a list of files linking to FILE."
  (let ((file (expand-file-name file)))
    (mapcar
     (lambda (entry) (plist-get entry :file))
     (org-el-cache-select
      org-zk-cache
      (lambda (filename entry)
        (--any
         (string= (expand-file-name (plist-get it :path)) file)
         (plist-get entry :links)))))))

(defun org-zk-files-linking-here ()
  "Generate a list of files linking to the current buffer."
  (org-zk-files-linking-to (buffer-file-name)))

;;;; Updating / Deleting Links

(defvar org-zk-deleted-link-text "(deleted)")

;; TODO: Jump to file for manual fixes
;;
;; NOTE: It's easier to parse the buffer again instead of adding link bounds
;; to the org cache and dealing with cache-incoherence problems.
(defun org-zk-delete-link (target)
  "Replace links to TARGET with their description.
If a link has no description, replace it with
  `org-zk-deleted-link-text'.
Link targets are compared using their *absolute* path."
  (let (links
        (dir (file-name-directory (buffer-file-name))))
    (org-element-map
        (org-element-parse-buffer)
        'link
      (lambda (link)
        (if (string=
             (expand-file-name (org-element-property :path link))
             (expand-file-name target))
            (push link links))))
    ;; LINKS is already in reverse order so its save to delete links
    ;; by their bounds
    (dolist (link links)
      (if (org-element-contents link)
          (progn
            (goto-char (org-element-property :begin link))
            (delete-region
             (org-element-property :begin link)
             (org-element-property :end link))
            (insert
             (org-element-interpret-data
              (org-element-contents link)))
            ;; NOTE: There is some weird issue with :end being set incorrectly,
            ;; if the link doesn't end at the end of a line
            (unless (eolp) (insert " ")))
        (progn
          (goto-char (org-element-property :begin link))
          (delete-region
           (org-element-property :begin link)
           (org-element-property :end link))
          ;; NOTE: There is some weird issue with :end being set incorrectly
            ;; if the link doesn't end at the end of a line
          (insert
           org-zk-deleted-link-text)
          (unless (eolp) (insert " ")))))
    links))

(defun org-zk-update-link (target target-new desc-new)
  "Update links to TARGET to point to TARGET-NEW and change their description to DESC-NEW. DESC-NEW is set for links without a description, too.
Link targets are compared using their *absolute* path.
If either TARGET-NEW or DESC-NEW is nil, that part of the link is left unchanged."
  (let (links
        (dir (file-name-directory (buffer-file-name))))
    (org-element-map
        (org-element-parse-buffer)
        'link
      (lambda (link)
        (if (string=
             (expand-file-name (org-element-property :path link))
             (expand-file-name target))
            (push link links))))
    ;; LINKS is already in reverse order so its save to delete links
    ;; by their bounds
    (dolist (link links)
      (if desc-new
          (org-element-set-contents link desc-new))
      (if target-new
          (org-element-put-property link :path target-new))
      (goto-char (org-element-property :begin link))
      (delete-region
       (org-element-property :begin link)
       ;; Because of the way `org-element-interpret-data' work, there
       ;; is no need to use a -1 here
       (org-element-property :end link))
      (insert (org-element-interpret-data link)))
    links))

;;;; TODO Linking External Resources

;; TODO: Get title from url
;; TODO: Replace to add links to a resources heading
(defun org-zk-add-yank-link ()
  "Check if the element in the clipboard / kill-ring is an URL,
if so, insert a link to it in the edge list, prompting for a description."
  (interactive)
  (if-let ((res (with-temp-buffer
                  (yank)
                  (thing-at-point 'url (point)))))
      (org-zk-add-plain-link res (read-string "Description: "))
    (message "Kill-ring contents are not a URL")))
;;; Creating / Renaming Files

(defun org-zk-read-title ()
  "Read a string to use as document title."
  (org-zk-titlecase (string-trim (read-string "Title: "))))

(defun org-zk-new-file (&optional title)
  "Create a new org zettelkasten file.
Prompts for a title, and a project, then uses the projects
name-fn to generate a filename."
  (interactive)
  (org-zk-select-collection
   (lambda (c)
     (let* ((collection (cdr c))
            (title (or title (org-zk-read-title)))
            (name-fn (org-zk-collection-name-fn collection))
            (setup-fn (org-zk-collection-setup-fn collection))
            (name (funcall name-fn title))
            (file (expand-file-name
                   (concat name ".org")
                   (plist-get collection :path))))
       (if (file-exists-p file)
           (error "Aborting, file already exists: %s" file))
       (find-file file)
       (funcall setup-fn title)
       (save-buffer)))))

(defun org-zk-create-file (title collection)
  "Create a new org zettelkasten file given its collection and title.
Returns the name of the new file"
  (let* ((name-fn (org-zk-collection-name-fn collection))
         (setup-fn (org-zk-collection-setup-fn collection))
         (name (funcall name-fn title))
         (file (expand-file-name
                (concat name ".org")
                (plist-get collection :path))))
    (if (file-exists-p file)
        (error "Aborting, file already exists: %s" file))
    (with-current-buffer (find-file-noselect file)
      (funcall setup-fn title)
      (save-buffer)
      (kill-buffer))
    file))

(defun org-zk-rename (title)
  "Rename the current zettel, prompting for a new TITLE."
  (interactive (list (org-zk-read-title)))
  (org-zk-keywords-set-or-add "TITLE" title)
  (let ((target (buffer-file-name)))
    (dolist (file (org-zk-files-linking-here))
      (org-zk-in-file (car file)
        (org-zk-update-link target nil title)))))

;;; Deleting / Slurping Files

(defun org-zk-delete-file (file)
  "Delete FILE, removing all links to it."
  (let ((file (expand-file-name file)))
    (dolist (entry (org-zk-files-linking-to file))
      (org-zk-in-file (plist-get entry :file)
        (org-zk-delete-link file)
        (save-buffer)
        (kill-buffer))))
  (delete-file file))

(defun org-zk--file-to-headline (file)
  "Convert the contents of FILE to a headline in the current
buffer"
  (org-insert-heading nil nil t)
  (insert (org-zk-file-title file) "\n")
  (insert-file-contents
   file
   nil
   (org-zk--beginning-of-main-content file))
  (org-zk-delete-file file))

(defun org-zk-slurp ()
  "Select a file, then insert it as a headline in the current
buffer."
  (interactive)
  (org-zk-select-file
   (lambda (selection) (org-zk--file-to-headline (cdr selection)))))

;;; (Changing) File Keywords

(defun org-zk-keywords-used ()
  "List of all keywords used."
  (let (keywords)
    (org-el-cache-each
     org-zk-cache
     (lambda (_filename entry)
       (dolist (kw (plist-get entry :keywords))
         (if (not (member kw keywords))
             (push kw keywords)))))
    keywords))

(defun org-zk-add-keyword (kw)
  "Add a keyword to the current buffer."
  (interactive (list (org-zk-read-keyword)))
  (org-zk-keywords-list-add "KEYWORDS" kw))

(defun org-zk-edit-keywords (kw-string)
  "Edit keywords of the current buffer."
  (interactive (list (read-string
                      "Keywords: "
                      (org-zk-keywords-get "KEYWORDS"))))
  (org-zk-keywords-set-or-add "KEYWORDS" kw-string))

(defun org-zk-read-keyword ()
  "Read a keyword."
  (ivy-completing-read "Keyword: " (org-zk-keywords-used)))

;;; Agenda Hacks

(defun org-zk--has-todos (entry)
  (plusp
   (count-if
    (lambda (hl)
  (or (string= (plist-get hl :todo-keyword) "NEXT")
          (string= (plist-get hl :todo-keyword) "TODO")))
    (plist-get entry :headlines))))

(defun org-zk--agenda-files ()
  (let ((files))
    (org-el-cache-each
     org-zk-cache
     (lambda (key value)
       (let* ((keywords (plist-get value :org-keywords))
              (state (alist-get "GTD_STATE" keywords nil nil 'string=)))
         (if (or
              (string= state "active")
              (and (null state) (org-zk--has-todos value)))
             (push key files)))))
    files))

(defun org-zk-agenda ()
  (interactive)
  (setq org-agenda-files (org-zk--agenda-files))
  (org-agenda))

(defun org-zk-agenda-list ()
  (interactive)
  (setq org-agenda-files (org-zk--agenda-files))
  (org-agenda-list))

(defun org-zk-todo-list ()
  (interactive)
  (setq org-agenda-files (org-zk--agenda-files))
  (org-todo-list))

(defun org-zk-ql-next ()
  (interactive)
  (org-ql-search
    (org-zk--agenda-files)
    '(todo "NEXT")))

(defun org-zk-slurp ()
  "Select a file, then insert it as a headline in the current
buffer."
  (interactive)
  (org-zk-select-file
   (lambda (selection) (org-zk--file-to-headline (cdr selection)))))

(defun org-zk-delete-file (file)
  "Delete FILE, removing all links to it."
  (let ((file (expand-file-name file)))
    (dolist (entry (org-zk-files-linking-to file))
      (org-zk-in-file (plist-get entry :file)
        (org-zk-delete-link file)
        (save-buffer)
        (kill-buffer))))
  (delete-file file))

;;; Exports

(provide 'org-zk-core)
