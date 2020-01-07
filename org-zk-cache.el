(require 'subr-x)

(defvar org-zk-cache--table
  (make-hash-table
   :test 'equal
   :size 1000))

(defun org-zk-cache-get (path)
  (gethash path org-zk-cache--table))

(defclass org-zk-cache-file ()
  ((path :initarg :path)
   (hash :initarg :hash)
   (keywords :initarg :keywords)
   (headlines :initarg :headlines)
   ;; (priority :initarg :priority)
   (links :initarg :links)
   (category :initarg :category)))

(defun org-zk-cache-file-title (cache-file)
  (org-zk-cache-get-keyword
   cache-file
   "TITLE"
   (oref cache-file path)))

(defun make-org-zk-cache-file (path hash)
  (make-instance 'org-zk-cache-file
                 :path path
                 :hash hash
                 :keywords nil
                 :headlines nil
                 :links nil
                 :category (org-zk-category-for-file path)))

(defclass org-zk-cache-headline ()
  ((begin :initarg :begin)
   (parent :initarg :parent)
   (deadline :initarg :deadline)
   (scheduled :initarg :scheduled)
   (timestamps :initarg :timestamps)
   (level :initarg :level)
   (priority :initarg :priority)
   (tags :initarg :tags)
   (title :initarg :title)
   (todo-keyword :initarg :todo-keyword)
   (todo-type :initarg :todo-type)
   (effort :initarg :effort)
   (style :initarg :style)
   (clocks :initarg :clocks)))

(defclass org-zk-cache-link ()
  ((type :initarg :type)
   (parent :initarg :parent)
   (path :initarg :path)
   (text :initarg :text)))

(defun org-zk-cache-link-from-element (parent el)
  (make-instance 'org-zk-cache-link
   :type (org-element-property :type el)
   :path (org-element-property :path el)
   :parent parent
   :text (org-element-interpret-data (org-element-contents el))))

(defclass org-zk-cache-timestamp ()
  ((type :initarg :type)
   (unit :initarg :unit)
   (value :initarg :value)
   (ts :initarg :ts)))

(defun make-org-zk-cache-timestamp (ts type unit value)
  (make-instance 'org-zk-cache-timestamp
                 :ts ts
                 :type type
                 :unit unit
                 :value value))

(defun org-zk-cache--ts-from-element (el)
  (make-ts
   :year (org-element-property :year-start el)
   :month (org-element-property :month-start el)
   :day (org-element-property :day-start el)
   :hour (or (org-element-property :hour-start el) 0)
   :minute (or (org-element-property :minute-start el) 0)
   :second 0))

(defun org-zk-cache--ts-from-element-end (el)
  (make-ts
   :year (org-element-property :year-end el)
   :month (org-element-property :month-end el)
   :day (org-element-property :day-end el)
   :hour (or (org-element-property :hour-end el) 0)
   :minute (or (org-element-property :minute-end el) 0)
   :second 0))

(defun org-zk-cache-timestamp-from-element (el type)
  (if el
      (make-instance 'org-zk-cache-timestamp
                     :ts (org-zk-cache--ts-from-element el)
                     :type type
                     :unit (org-element-property :repeater-unit el)
                     :value (org-element-property :repeater-value el))))

(defclass org-zk-cache-clock ()
  ((begin :initarg :begin)
   (end :initarg :end)
   (status :initarg :status)))

(defun org-zk-cache-clock-from-element (element)
  (let ((value (org-element-property :value element)))
    (if (eq (org-element-property :status element) 'closed)
        (make-instance
         'org-zk-cache-clock
         :begin (org-zk-cache--ts-from-element value)
         :end (org-zk-cache--ts-from-element-end value)
         :status 'closed)
      (make-instance
         'org-zk-cache-clock
         :begin (org-zk-cache--ts-from-element value)
         :end nil
         :status 'running))))

(defmethod org-zk-cache-clock-duration ((clock org-zk-cache-clock))
  "Duration of CLOCK in seconds."
  (ts-diff
   (or (oref clock end) (ts-now))
   (oref clock begin)))

(defmethod org-zk-cache-clock-duration-in-range ((clock org-zk-cache-clock) from to)
  "Duration of CLOCK in seconds."
  (let* ((begin (oref clock begin))
         (end (or (oref clock end) (ts-now)))
         (begin-cut (if (ts<= begin from) from begin))
         (end-cut (if (ts>= end to) to end))
         ;; With the way begin-cut and end-cut work, timestamps fully
         ;; outside the range from-to are "flipped" so that end <
         ;; begin
         (diff (ts-diff end-cut begin-cut)))
    (if (plusp diff) diff 0)))

(defun org-zk-cache-files ()
  "Return a list of all files managed by org-zk-cache"
  (hash-table-keys org-zk-cache--table))

(defun org-zk-cache--headline-timestamps (element)
  (mapcar
   (lambda (el) (org-zk-cache-timestamp-from-element el 'plain))
   (remove-if-not
    (lambda (e)
      (or (eq (org-element-property :type e) 'active)
          (eq (org-element-property :type e) 'active-range)))
    (org-element-map
        (assoc 'section (org-element-contents element))
        'timestamp
      #'identity))))

(defun org-zk-cache--headline-clocks (element)
  (org-element-map
      element
      'clock
    #'org-zk-cache-clock-from-element))

(defun org-zk-cache-headline-from-element (parent element)
  (make-instance
   'org-zk-cache-headline
   :parent parent
   :begin (org-element-property :begin element)
   :deadline (org-zk-cache-timestamp-from-element
              (org-element-property :deadline element)
              'deadline)
   :scheduled (org-zk-cache-timestamp-from-element
               (org-element-property :scheduled element)
               'scheduled)
   :timestamps (org-zk-cache--headline-timestamps element)
   :level (org-element-property :level element)
   :priority (org-element-property :priority element)
   :tags (org-element-property :tags element)
   :todo-keyword (org-element-property :todo-keyword element)
   :todo-type (org-element-property :todo-type element)
   :title (org-element-property :raw-value element)
   :effort (org-element-property :EFFORT element)
   :style (org-element-property :STYLE element)
   :clocks (org-zk-cache--headline-clocks element)))

(defun org-zk-cache-process-buffer (&optional path)
  (interactive)
  (let* ((path (or path (buffer-file-name)))
         (entry (org-zk-cache-get path))
         (buffer-hash (buffer-hash))
         (cat (org-zk-category-for-file path)))
    (if (and cat (not (org-zk-category-ignore-p cat)))
        (when (or (null entry)
                  (not (string= buffer-hash (oref entry hash))))
          (let ((element (org-element-parse-buffer))
                (object (make-org-zk-cache-file path buffer-hash)))
            (puthash
             path
             (org-zk-cache-process-element element object)
             org-zk-cache--table))))))

;; Because we would have to enable org-mode anyway,
;; using `with-temp-buffer` and `insert-file-contents`
;; is not faster than using `with-current-buffer`.
(defun org-zk-cache-process-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (org-mode)
    (org-zk-cache-process-buffer path)))

(defun org-zk-cache-remove-file (path)
  (remhash path org-zk-cache--table))

(defun org-zk-cache-remove-nonexisting-files ()
  "Remove non-existing files from the database"
  (interactive)
  (dolist (path (hash-table-keys org-zk-cache--table))
    (unless (file-exists-p path)
      (message "Removing file %s from cache" path)
      (org-zk-cache-remove-file path))))

(cl-defmethod org-zk-cache-add-keyword ((cache-file org-zk-cache-file) key value)
  (push (cons key value) (oref cache-file keywords)))

(cl-defmethod org-zk-cache-add-headline ((cache-file org-zk-cache-file) (headline org-zk-cache-headline))
  (push headline (oref cache-file headlines)))

(cl-defmethod org-zk-cache-add-link ((cache-file org-zk-cache-file) (link org-zk-cache-link))
  (push link (oref cache-file links)))

(cl-defmethod org-zk-cache-get-keyword ((cache-file org-zk-cache-file) key &optional default)
  (alist-get
   key
   (oref cache-file keywords)
   default
   nil
   #'string-equal))

(defun org-zk-cache-process-element (element object)
  (org-zk-cache-process-first-section
   (car (org-element-contents element))
   object)
  (org-element-map
      element
      'headline
    (lambda (headline) (org-zk-cache-process-headline headline object)))
  (org-element-map
      element
      'link
    (lambda (link) (org-zk-cache-process-link link object)))
  object)

(defun org-zk-cache-process-first-section (element object)
  (if (eq (org-element-type element) 'section)
      (dolist (child (org-element-contents element))
        (org-zk-cache-process-keyword child object))))

(defun org-zk-cache-process-keyword (element object)
  (if (eq (org-element-type element) 'keyword)
      (org-zk-cache-add-keyword
       object
       (org-element-property :key element)
       (org-element-property :value element))))

; See: https://orgmode.org/worg/dev/org-element-api.html
(defun org-zk-cache-process-headline (headline object)
  (org-zk-cache-add-headline object (org-zk-cache-headline-from-element object headline)))

(defun org-zk-cache-process-link (link object)
  (org-zk-cache-add-link object (org-zk-cache-link-from-element object link)))

;; TODO: Use category information
(defun org-zk-cache-create ()
  (interactive)
  (dolist (file (org-zk-files-with-categories))
    (org-zk-cache-process-file (car file))))

;; TODO: Better handling of archive files
(defun org-zk-archive-files ()
  (--filter (string-suffix-p "archive" it) (hash-table-keys org-zk-cache--table)))

;; TODO: Better handling of archive files
(defun org-zk-remove-archive-files ()
  (--map (remhash it org-zk-cache--table) (org-zk-archive-files)))

(defun org-zk-cache-clear ()
  (interactive)
  (clrhash org-zk-cache--table))

(defun org-zk-cache-recreate ()
  (interactive)
  (org-zk-cache-clear)
  (org-zk-cache-create))

(defun org-zk-cache-map (fn)
  "Apply FN to each file entry in the cache.
FN is called with two arguments: the file path and the cached file object"
  (let (entries)
    (maphash
     (lambda (key value) (push (funcall fn key value) entries))
     org-zk-cache--table)
    entries))

(defun org-zk-cache-mapcan (fn)
  "Apply FN to each file entry in the cache.
FN is called with two arguments: the file path and the cached file object"
  (let (entries)
    (maphash
     (lambda (key value) (setq entries (nconc entries (funcall fn key value))))
     org-zk-cache--table)
    entries))

(defun org-zk-cache-filter (pred)
  "Collect a list of (path . cached-file) pairs matching the predicate PRED.
FN is called with two arguments: the file path and the cached file object"
  (let (entries)
    (maphash
     (lambda (key value)
       (if (funcall pred key value)
           (push
            (cons key value)
            entries)))
     org-zk-cache--table)
    entries))

;; TODO: There has to be a better way
(defun org-zk-cache-save (path)
  (with-current-buffer (find-file-noselect path)
    (insert (with-output-to-string (prin1 org-zk-cache--table)))
    (save-buffer)))

(defun org-zk-cache-load (path)
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (read (current-buffer))))

(defun org-zk-cache-hash-file (path)
   (shell-command-to-string
    (concat "md5sum" path " | awk '{print $1}'")))

(defun org-zk-cache-hash-file2 (path)
  (with-temp-buffer
    (insert-file-contents path)
    (secure-hash 'md5 (buffer-string))))

(defvar org-zk-cache-file-query-macros nil)

(defmacro define-org-zk-cache-file-query-macro (name args expansion)
  `(add-to-list
    'org-zk-cache-file-query-macros
    (cons (quote ,name) (lambda ,args ,expansion))))

;; (keyword key value)
(defun org-zk-cache-compile-file-query (query)
  (if (null query)
      t
    (case (car query)
      ('keyword
       `(let ((keyword-value (org-zk-cache-get-keyword entry ,(cadr query))))
          (and keyword-value
               (string-equal keyword-value ,(caddr query)))))
      ('or `(or ,@(mapcar #'org-zk-cache-compile-file-query (cdr query))))
      ('and `(and ,@(mapcar #'org-zk-cache-compile-file-query (cdr query))))
      (t (let ((macro (alist-get (car query) org-zk-cache-file-query-macros)))
           (if macro
               (org-zk-cache-compile-file-query (apply macro (cdr query)))
               (error "Invalid query function %s" (car query))))))))

(defun org-zk-cache-compile-headline-query (query)
  (if (null query)
      t
    (case (car query)
      ('todo
       `(string-equal ,(cadr query)
                      (oref headline todo-keyword)))
      ('or `(or ,@(mapcar #'org-zk-cache-compile-headline-query (cdr query))))
      ('and `(and ,@(mapcar #'org-zk-cache-compile-headline-query (cdr query))))
      (t (error "Invalid query function %s" (car query))))))

(defun org-zk-cache-file-query (query)
  (org-zk-cache-filter
   `(lambda (path entry) ,(org-zk-cache-compile-file-query query))))

(defun org-zk-cache-headline-query (file-query headline-query)
  (let (result
        (file-query
         `(lambda (path entry) ,(org-zk-cache-compile-file-query file-query)))
        (headline-query
         `(lambda (headline) ,(org-zk-cache-compile-headline-query headline-query))))
     (dolist (entry (org-zk-cache-filter file-query))
       (dolist (headline (oref (cdr entry) headlines))
         (if (funcall headline-query headline)
             (push headline result))))
    result))

(defun org-zk-cache-file-headline-query (entry headline-query)
  (let (result
        (headline-query
         `(lambda (headline) ,(org-zk-cache-compile-headline-query headline-query))))
    (dolist (headline (oref entry headlines))
      (if (funcall headline-query headline)
          (push headline result)))
    result))

(defun org-zk-cache-remove-file-advice (path &optional _trash)
  (message "Removing cached file %s" (file-truename path))
  (org-zk-cache-remove-file (file-truename path)))

(advice-add 'delete-file :before #'org-zk-cache-remove-file-advice)

(defun org-zk-cache-rename-file-advice (old-path new-path &optional _ok-if-already-exists)
  (unless (string-prefix-p (temporary-file-directory) new-path)
    (let* ((old-path (file-truename old-path))
           (new-path (file-truename new-path))
           (entry (org-zk-cache-get old-path)))
      (when entry
        (message "Renaming cached file %s to %s" old-path new-path)
        (oset entry category (org-zk-category-for-file new-path))
        (puthash new-path entry org-zk-cache--table)
        (remhash old-path org-zk-cache--table)))))

(advice-add 'rename-file :before #'org-zk-cache-rename-file-advice)

;; Check buffer = dirty? instead
;; (defun org-zk-cache-advice (&rest _args)
;;   (interactive)
;;   (org-zk-cache-process-buffer))
;;
;; (advice-add 'org-todo :after #'org-zk-cache-advice)
;; (advice-add 'org-clock-in :after #'org-zk-cache-advice)
;; (advice-add 'org-clock-out :after #'org-zk-cache-advice)

;; Org brain speedup code

(defun org-brain--file-targets (file)
  (let* ((file-relative (org-brain-path-entry-name file))
         (file-entry-name (org-brain-entry-name file-relative)))
    (cons file-entry-name file-relative)))

(defun org-brain-keywords (entry)
  "Get alist of `org-mode' keywords and their values in file ENTRY."
  (if (org-brain-filep entry)
      (let ((cache-entry (org-zk-cache-get (org-brain-entry-path entry))))
        (if cache-entry
            (oref cache-entry :keywords)
            (with-temp-buffer
              (insert
               (with-temp-buffer
                 (ignore-errors (insert-file-contents (org-brain-entry-path entry)))
                 (buffer-substring-no-properties (point-min) (org-brain-first-headline-position))))
              (org-element-map (org-element-parse-buffer) 'keyword
                (lambda (kw)
                  (cons (org-element-property :key kw)
                        (org-element-property :value kw)))))))
    (error "Only file entries have keywords")))

;; (buffer-file-name (car (buffer-list)))
;; using `buffer-modified-p` I can check if the buffer is modified
;; include this in org-zk-cache-get, check if there is a dirty buffer for this file,
;; if so, re-scan it.
;;
;; In mapping functions, iterate over all dirty buffers & re-process them first.

(provide 'org-zk-cache)
