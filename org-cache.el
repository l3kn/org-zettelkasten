(require 'subr-x)

(defvar org-cache--table
  (make-hash-table
   :test 'equal
   :size 1000))

(defun org-cache-get (path)
  (gethash path org-cache--table))

(defclass org-cache-file ()
  ((path :initarg :path)
   (hash :initarg :hash)
   (keywords :initarg :keywords)
   (headlines :initarg :headlines)
   (category :initarg :category)))

(defun make-org-cache-file (path hash)
  (make-instance 'org-cache-file
                 :path path
                 :hash hash
                 :keywords nil
                 :headlines nil
                 :category (org-zk-category-for-file path)))

(defclass org-cache-headline ()
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
   (style :initarg :style)))

(defclass org-cache-timestamp ()
  ((type :initarg :type)
   (unit :initarg :unit)
   (value :initarg :value)
   (ts :initarg :ts)))

(defun make-org-cache-timestamp (ts type unit value)
  (make-instance 'org-cache-timestamp
                 :ts ts
                 :type type
                 :unit unit
                 :value value))

(defun org-cache--ts-from-element (el)
  (make-ts
   :year (org-element-property :year-start el)
   :month (org-element-property :month-start el)
   :day (org-element-property :day-start el)
   :hour (or (org-element-property :hour-start el) 0)
   :minute (or (org-element-property :minute-start el) 0)
   :second 0))

(defun org-cache-timestamp-from-element (el type)
  (if el
      (make-instance 'org-cache-timestamp
                     :ts (org-cache--ts-from-element el)
                     :type type
                     :unit (org-element-property :repeater-unit el)
                     :value (org-element-property :repeater-value el))))

(defun org-cache-files ()
  (hash-table-keys org-cache--table))

(defun org-cache--headline-timestamps (element)
  (mapcar
   (lambda (el) (org-cache-timestamp-from-element el 'plain))
   (remove-if-not
    (lambda (e)
      (or (eq (org-element-property :type e) 'active)
          (eq (org-element-property :type e) 'active-range)))
    (org-element-map
        (assoc 'section (org-element-contents element))
        'timestamp
      #'identity))))

(defun org-cache-headline-from-element (parent element)
  (make-instance
   'org-cache-headline
   :parent parent
   :begin (org-element-property :begin element)
   :deadline (org-cache-timestamp-from-element
              (org-element-property :deadline element)
              'deadline)
   :scheduled (org-cache-timestamp-from-element
               (org-element-property :scheduled element)
               'scheduled)
   :timestamps (org-cache--headline-timestamps element)
   :level (org-element-property :level element)
   :priority (org-element-property :priority element)
   :tags (org-element-property :tags element)
   :todo-keyword (org-element-property :todo-keyword element)
   :todo-type (org-element-property :todo-type element)
   :title (org-element-property :raw-value element)
   :effort (org-element-property :EFFORT element)
   :style (org-element-property :STYLE element)))

(defun org-cache-process-buffer (&optional path)
  (let* ((path (or path (buffer-file-name)))
         (entry (org-cache-get path))
         (buffer-hash (buffer-hash)))
    (when (or (null entry)
              (not (string= buffer-hash (oref entry hash))))
      (message "processing buffer %s" path)
      (let ((element (org-element-parse-buffer))
            (object (make-org-cache-file path buffer-hash)))
        (puthash
         path
         (org-cache-process-element element object)
         org-cache--table)))))

;; Because we would have to enable org-mode anyway,
;; using `with-temp-buffer` and `insert-file-contents`
;; is not faster than using `with-current-buffer`.
(defun org-cache-process-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (org-mode)
    (org-cache-process-buffer path)))

(defun org-cache-remove-file (path)
  (remhash path org-cache--table))

(defun org-cache-remove-nonexisting-files ()
  "Remove non-existing files from the database"
  (interactive)
  (dolist (path (hash-table-keys org-cache--table))
    (unless (file-exists-p path)
      (message "Removing file %s from cache" path)
      (org-cache-remove-file path))))

(cl-defmethod org-cache-add-keyword ((cache-file org-cache-file) key value)
  (push (cons key value) (oref cache-file keywords)))

(cl-defmethod org-cache-add-headline ((cache-file org-cache-file) (headline org-cache-headline))
  (push headline (oref cache-file headlines)))

(cl-defmethod org-cache-get-keyword ((cache-file org-cache-file) key)
  (alist-get
   key
   (oref cache-file keywords)
   nil
   nil
   #'string-equal))

(defun org-cache-process-element (element object)
  (org-cache-process-first-section
   (car (org-element-contents element))
   object)
  (org-element-map
      element
      'headline
    (lambda (headline) (org-cache-process-headline headline object)))
  object)

(defun org-cache-process-first-section (element object)
  (if (eq (org-element-type element) 'section)
      (dolist (child (org-element-contents element))
        (org-cache-process-keyword child object))))

(defun org-cache-process-keyword (element object)
  (if (eq (org-element-type element) 'keyword)
      (org-cache-add-keyword
       object
       (org-element-property :key element)
       (org-element-property :value element))))

; See: https://orgmode.org/worg/dev/org-element-api.html
(defun org-cache-process-headline (headline object)
  (org-cache-add-headline object (org-cache-headline-from-element object headline)))

;; TODO: Use category information
(defun org-cache-create ()
  (interactive)
  (dolist (file (org-zk-files-with-categories))
    (org-cache-process-file (car file))))

(defun org-cache-clear ()
  (interactive)
  (clrhash org-cache--table))

(defun org-cache-recreate ()
  (interactive)
  (org-cache-clear)
  (org-cache-create))

(defun org-cache-map (fn)
  "Apply FN to each file entry in the cache.
FN is called with two arguments: the file path and the cached file object"
  (let (entries)
    (maphash
     (lambda (key value) (push (funcall fn key value) entries))
     org-cache--table)
    entries))

(defun org-cache-mapcan (fn)
  "Apply FN to each file entry in the cache.
FN is called with two arguments: the file path and the cached file object"
  (let (entries)
    (maphash
     (lambda (key value) (setq entries (nconc entries (funcall fn key value))))
     org-cache--table)
    entries))

(defun org-cache-filter (pred)
  "Collect a list of (path . cached-file) pairs matching the predicate PRED.
FN is called with two arguments: the file path and the cached file object"
  (let (entries)
    (maphash
     (lambda (key value)
       (if (funcall pred key value)
           (push
            (cons key value)
            entries)))
     org-cache--table)
    entries))

;; TODO: There has to be a better way
(defun org-cache-save (path)
  (with-current-buffer (find-file-noselect path)
    (insert (with-output-to-string (prin1 org-cache--table)))
    (save-buffer)))

(defun org-cache-load (path)
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (read (current-buffer))))

(defun org-cache-hash-file (path)
   (shell-command-to-string
    (concat "md5sum" path " | awk '{print $1}'")))

(defun org-cache-hash-file2 (path)
  (with-temp-buffer
    (insert-file-contents path)
    (secure-hash 'md5 (buffer-string))))

(defvar org-cache-file-query-macros nil)

(defmacro define-org-cache-file-query-macro (name args expansion)
  `(add-to-list
    'org-cache-file-query-macros
    (cons (quote ,name) (lambda ,args ,expansion))))

;; (keyword key value)
(defun org-cache-compile-file-query (query)
  (if (null query)
      t
    (case (car query)
      ('keyword
       `(let ((keyword-value (org-cache-get-keyword entry ,(cadr query))))
          (and keyword-value
               (string-equal keyword-value ,(caddr query)))))
      ('or `(or ,@(mapcar #'org-cache-compile-file-query (cdr query))))
      ('and `(and ,@(mapcar #'org-cache-compile-file-query (cdr query))))
      (t (let ((macro (alist-get (car query) org-cache-file-query-macros)))
           (if macro
               (org-cache-compile-file-query (apply macro (cdr query)))
               (error "Invalid query function %s" (car query))))))))

(defun org-cache-compile-headline-query (query)
  (if (null query)
      t
    (case (car query)
      ('todo
       `(string-equal ,(cadr query)
                      (oref headline todo-keyword)))
      ('or `(or ,@(mapcar #'org-cache-compile-headline-query (cdr query))))
      ('and `(and ,@(mapcar #'org-cache-compile-headline-query (cdr query))))
      (t (error "Invalid query function %s" (car query))))))

(defun org-cache-file-query (query)
  (org-cache-filter
   `(lambda (path entry) ,(org-cache-compile-file-query query))))

(defun org-cache-headline-query (file-query headline-query)
  (let (result
        (file-query
         `(lambda (path entry) ,(org-cache-compile-file-query file-query)))
        (headline-query
         `(lambda (headline) ,(org-cache-compile-headline-query headline-query))))
     (dolist (entry (org-cache-filter file-query))
       (dolist (headline (oref (cdr entry) headlines))
         (if (funcall headline-query headline)
             (push headline result))))
    result))

(defun org-cache-file-headline-query (entry headline-query)
  (let (result
        (headline-query
         `(lambda (headline) ,(org-cache-compile-headline-query headline-query))))
    (dolist (headline (oref entry headlines))
      (if (funcall headline-query headline)
          (push headline result)))
    result))

(defun delete-file-org-cache-remove-file (path &optional _trash)
  (message "Removing cached file %s" (file-truename path))
  (org-cache-remove-file (file-truename path)))

(advice-add 'delete-file :before #'delete-file-org-cache-remove-file)

(defun rename-file-org-cache-rename-file (old-path new-path &optional _ok-if-already-exists)
  (unless (string-prefix-p (temporary-file-directory) new-path)
    (let* ((old-path (file-truename old-path))
           (new-path (file-truename new-path))
           (entry (org-cache-get old-path)))
      (when entry
        (message "Renaming cached file %s to %s" old-path new-path)
        (oset entry category (org-zk-category-for-file new-path))
        (puthash new-path entry org-cache--table)
        (remhash old-path org-cache--table)))))

(advice-add 'rename-file :before #'rename-file-org-cache-rename-file)

(provide 'org-cache)
