;;; Customization

(defcustom org-zk-alias-keyword "ZK_ALIAS"
  "Keyword for file title aliases")

(defvar org-zk-gtd-states
  '("active"
    "someday"
    "planning"
    "cancelled"
    "done"))

(defvar org-zk-default-file-state "active")
(defvar org-zk-gtd-state-keyword "GTD_STATE")

(defcustom org-zk-directory "~/org"
  "Directory to treat as a Zettelkasten."
  :type 'string
  :group 'org-zk)


;;; Helper Functions / Macros

(defun org-zk-member-p (file)
  "Check if FILE is part of org-zk."
  (string-prefix-p
   (expand-file-name (file-truename org-zk-directory))
   (expand-file-name (file-truename file))))

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
           (save-excursion
             (prog1 (progn ,@body)
               (org-el-cache-process-buffer))))
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

(defun org-zk-buffer-file-name ()
  "Wrapper around `buffer-file-name' that works in capture buffers."
  (buffer-file-name (buffer-base-buffer)))

(defun org-zk-buffer-member-p ()
  "Check if the current buffer is part of org-zk."
  (if-let ((filename (org-zk-buffer-file-name)))
   (org-zk-member-p filename)))

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
  (insert "#+DATE: ")
  (org-insert-time-stamp (current-time) t t)
  (insert "\n\n"))

(defvar org-zk-file-blacklist '("./" "../" ".git/"))

(defvar org-zk--collections '())

(defun org-zk-set-collections (collections)
  "Set the zettelkasten collections to COLLECTIONS.
This should be used instead of setting the variable directly
because collection paths need to be in canonical form."
  (setq org-zk--collections
        (mapcar (lambda (c)
                  (plist-put
                   c :path
                   (expand-file-name (plist-get c :path))))
                collections)))

(defun org-zk-collection-ignore-p (p) (plist-get p :ignore))
(defun org-zk-collection-name-fn (p)
  (getf p :name-fn 'org-zk-default-name-fn))
(defun org-zk-collection-setup-fn (p)
  (getf p :setup-fn 'org-zk-default-setup-fn))

(defun org-zk-collection-for-file (filename)
  "Find the (sub)collection FILENAME belongs to.
Returns NIL if FILENAME is not managed by org-zettelkasten."
  (let ((filename (expand-file-name filename)))
    (seq-find
     (lambda (c) (string-prefix-p (plist-get c :path) filename))
     org-zk--collections)))

(defun org-zk-select-collection (action)
  "Select a collection by its name,
then call ACTION with the collection that was selected."
  (interactive)
  (let ((collection
         (mapcar
          (lambda (c) (cons (plist-get c :name) c))
          org-zk--collections)))
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

(defclass org-zk-clock ()
  ((begin :initarg :begin)
   (end :initarg :end)
   (status :initarg :status)))

(defun org-zk-clock-from-element (element)
  (let ((value (org-element-property :value element)))
    (if (eq (org-element-property :status element) 'closed)
        (make-instance
         'org-zk-clock
         :begin (org-zk-time--ts-from-element value)
         :end (org-zk-time--ts-from-element-end value)
         :status 'closed)
      (make-instance
         'org-zk-clock
         :begin (org-zk-time--ts-from-element value)
         :end nil
         :status 'running))))

(defun org-zk--cache-headlines (el filetags)
  (let ((itags (org-remove-uninherited-tags filetags)))
    (org-element-map el 'headline
      (lambda (child)
        (list
         :begin (org-element-property :begin child)
         :deadline (org-element-property :deadline child)
         :scheduled (org-element-property :scheduled child)
         :level (org-element-property :level child)
         :priority (org-element-property :priority child)
         :tags (delete-dups
                (append itags (org-element-property :tags child)))
         :todo-keyword (org-element-property :todo-keyword child)
         :todo-type (org-element-property :todo-type child)
         :title (org-element-property :raw-value child)
         :id (org-element-property :ID child)
         :effort (org-element-property :EFFORT child)
         ;; Work on element contents & don't recurse into headlines
         ;; to extract only the "direct" clocks of a headline
         :clocks (org-element-map (org-element-contents child) 'clock
                   #'org-zk-clock-from-element
                   nil nil 'headline)
         :style (org-element-property :STYLE child))))))

;;; Cache Setup

(def-org-el-cache org-zk-cache (list org-zk-directory)
  (lambda (filename el)
    (let* ((org-keywords (org-zk--cache-keywords el))
           (collection (org-zk-collection-for-file filename))
           (filetags (split-string (alist-get "FILETAGS" org-keywords "" nil #'string=) ":" t)))
      (list
       :file filename
       :links (org-zk--cache-links el)
       :org-keywords org-keywords
       :keywords
       (split-string (alist-get "KEYWORDS" org-keywords "" nil #'string=) " " t)
       :created
       (alist-get "DATE" org-keywords nil nil #'string=)
       :published
       ;; TODO: Use keyword variable
       (alist-get "PUBLISHED" org-keywords nil nil #'string=)
       :title
       (or (alist-get "TITLE" org-keywords nil nil #'string=)
           (file-relative-name filename (plist-get collection :path)))
       :aliases
       (mapcar
        #'cdr
        (remove-if-not
         (lambda (kv) (string= (car kv) org-zk-alias-keyword))
         org-keywords))
       :collection (org-zk-collection-for-file filename)
       :headlines (org-zk--cache-headlines el filetags)))))

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
  (org-el-cache-update org-zk-cache))

(defun org-zk-cache-force-update ()
  "Force-update / recreate the zettelkasten cache."
  (interactive)
  (org-el-cache-clear org-zk-cache)
  (org-el-cache-update org-zk-cache))

;;;; Cache Update Hooks

(if (fboundp 'ace-window)
    (defadvice ace-window (before other-frame-now activate)
      (when buffer-file-name (org-el-cache-process-buffer))))

;;; File Selection

(defun org-zk-files ()
  "Return a list of all files managed by org-zettelkasten."
  (org-el-cache-map
   org-zk-cache
   (lambda (filename _entry) filename)))

(defun org-zk-files-with-titles ()
  "Return an alist of entries (title-with-collection . filename)"
  (org-el-cache-map
   org-zk-cache
   (lambda (filename entry)
     (let ((collection (plist-get entry :collection))
           (title (plist-get entry :title)))
       (if collection
           (cons
            (format "%s (%s)" title (plist-get collection :name))
            (cons title filename))
         (cons (format "%s" title) filename))))))

(defun org-zk-files-with-titles-and-aliases ()
  "Return an alist of entries (title-with-collection . filename).
Also treats file aliases as titles."
  (mapcar
   (lambda (file-title)
     (let ((collection (org-zk-collection-for-file (car file-title))))
       (if collection
           (cons
            (format "%s (%s)" (cadr file-title) (plist-get collection :name))
            (cons (cadr file-title) (car file-title)))
         (cons (format "%s" (cadr file-title))
               (cons (cadr file-title) (car file-title))))))
   (org-zk-awk-titles)))

(defun org-zk-files-with-titles-and-aliases_ ()
  "Return an alist of entries (title-with-collection . filename).
Also treats file aliases as titles."
  (org-el-cache-flatmap
   org-zk-cache
   (lambda (filename entry)
     (let* ((collection (plist-get entry :collection))
            (col-name (plist-get collection :name))
            (titles (cons (plist-get entry :title)
                          (plist-get entry :aliases))))
       (if collection
           (mapcar
            (lambda (title)
              (cons
               (format "%s (%s)" title col-name)
               (cons title filename)))
            titles)
         (mapcar
          (lambda (title) (cons (format "%s" title) (cons title filename)))
          titles))))))

(defvar org-zk-ivy-histoy nil)

(defun org-zk-select-file (action)
  (ivy-read
   "File: "
   (org-zk-files-with-titles-and-aliases)
   :history 'org-zk-ivy-history
   :action action))

(defun org-zk-open-file ()
  "Select a file, then open it"
  (interactive)
  (org-zk-select-file
   (lambda (selection)
     (if (stringp selection)
         (org-zk-new-file selection)
         (find-file (cddr selection))))))

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
     ;; Make sure there is a space in front of the link,
     ;; if that is necessary
     (unless (or (bolp)
                 (looking-at " ")
                 (looking-at "("))
       (insert " "))
     (if (stringp selection)
         (let* ((title (org-zk-titlecase (string-trim selection)))
                (target (org-zk-create-file
                         title
                         (org-zk-collection-for-file (buffer-file-name)))))
           (insert (org-zk-make-link
                    (file-relative-name target (file-name-directory (org-zk-buffer-file-name)))
                    title)))
       (insert (org-zk-make-link
                (file-relative-name
                 (cddr selection)
                 (file-name-directory (org-zk-buffer-file-name)))
                (cadr selection)))))))

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

(defun org-zk-read-title (&optional initial-input)
  "Read a string to use as document title."
  (org-zk-titlecase (string-trim (read-string "Title: " initial-input))))

(defun org-zk-new-file (&optional title)
  "Create a new org zettelkasten file.
Prompts for a title, and a project, then uses the projects
name-fn to generate a filename."
  (interactive)
  (org-zk-select-collection
   (lambda (c)
     (let* ((collection (cdr c))
            (title (if title
                       (org-zk-titlecase (string-trim title))
                     (org-zk-read-title)))
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
    (dolist (linking-file (org-zk-files-linking-here))
      (org-zk-in-file linking-file
        (org-zk-update-link target nil title)))))

;; TODO: Less duplication
(defun org-zk-rename-file (file title)
  "Change the title of FILE to TITLE.
This updates all links to FILE to use the new title as
description."
  (org-zk-in-file file
    (org-zk-keywords-set-or-add "TITLE" title))
  (dolist (linking-file (org-zk-files-linking-to file))
    (org-zk-in-file linking-file
      (org-zk-update-link file nil title))))

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
   (lambda (selection) (org-zk--file-to-headline (cddr selection)))))

;;; (Changing) File Keywords

(defun org-zk-set-file-keywords (file kws)
  (org-zk-in-file file
    (org-zk-keywords-set-or-add
     "KEYWORDS"
     (mapconcat #'identity kws " "))))

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

(defun org-zk-add-alias (alias)
  (interactive (list (read-string "Alias: ")))
  (org-zk-keywords-add org-zk-alias-keyword alias))

;;; Refile


(defun org-zk-slurp ()
  "Select a file, then insert it as a headline in the current
buffer."
  (interactive)
  (org-zk-select-file
   (lambda (selection) (org-zk--file-to-headline (cddr selection)))))

(defun org-zk-delete-file (file)
  "Delete FILE, removing all links to it."
  (let ((file (expand-file-name file)))
    (dolist (other-file (org-zk-files-linking-to file))
      (org-zk-in-file other-file
        (org-zk-delete-link file)
        (save-buffer)
        (kill-buffer))))
  (delete-file file))

;;; Exports

(provide 'org-zk-core)
