;;; Commentary
;; This package would not be complete without its own half-backed
;; implementation of a small lisp

;;; Compiler

(defun org-zk-query-tokenize (query)
  ;; A nice trick I got from http://www.norvig.com/lispy.html
  (setq query (replace-regexp-in-string (rx "(") " ( " query))
  (setq query (replace-regexp-in-string (rx ")") " ) " query))
  (split-string query " " t))

;; Returns a pair (parsed . rest)
(defun org-zk-query-parse (tokens acc inside)
  (cond
   ((null tokens)
    (if inside (error "org-zk: Unbalanced parentheses in query"))
    (cons (reverse acc) tokens))
   ((string= (car tokens) "(")
    ;; Read body of delimited expression, then process the rest
    (let ((res (org-zk-query-parse (cdr tokens) '() t)))
      (org-zk-query--read-delimited
       (cdr res)
       (cons (car res) acc)
       inside)))
   ((string= (car tokens) ")")
    (if (not inside) (error "org-zk: Unbalanced parentheses in query2"))
    (cons (reverse acc) (cdr tokens)))
   (t (org-zk-query-parse (cdr tokens) (cons (car tokens) acc) inside))))

(defun org-zk-query-compile-part (part predicates var)
  (if (listp part)
      (pcase (car part)
        ("and"
         `(and
           ,@(mapcar
              (lambda (part) (org-zk-query-compile-part part predicates var))
              (cdr part))))
        ("or"
         `(or
           ,@(mapcar
              (lambda (part) (org-zk-query-compile-part part predicates var))
              (cdr part))))
        (t (error "org-zk: Undefined query function %s" (car part))))
    (let ((tokens (split-string part ":" t)))
      (unless (= (length tokens) 2)
        (error "org-zk: Malformed query part"))
      (if-let ((predicate (alist-get (car tokens) predicates nil nil #'string=)))
          (funcall predicate (cadr tokens) var)
        (error "org-zk: Unknown query predicate %s" key)))))

(defun org-zk-query-compile (query predicates var)
  (cond
   ((null query) (lambda (_ _) t))
   ((null (cdr query))
    `(lambda (_filename ,var)
       ,(org-zk-query-compile-part (car query) predicates var)))
   (t
    `(lambda (_filename ,var)
       (and
        ,@(mapcar
           (lambda (part) (org-zk-query-compile-part part predicates var))
           parts))))))

(defun org-zk-query (query predicates)
  (let* ((tokens (org-zk-query-tokenize query))
         (parsed (car (org-zk-query-parse tokens '() nil)))
         (var (gensym)))
    (org-zk-query-compile parsed predicates var)))

;;; Predicates

(defun org-zk-query-pred-todo-keyword (arg var)
  `(equal (plist-get ,var :todo-keyword) ,arg))

(defun org-zk-query-pred-keyword (arg var)
  `(some (lambda (kw) (string= kw ,arg)) (plist-get ,var :keywords)))

(defun org-zk-query-pred-collection (arg var)
  `(string= ,arg (plist-get (plist-get ,var :collection) :name)))

(defun org-zk-query-pred-gtd-state (arg var)
  `(string= ,arg (org-zk-entry-gtd-state ,var)))

;;; Predicate Sets

(setq org-zk-query-hl-predicates
      `(("t" . ,#'org-zk-query-pred-todo-keyword)))

(setq org-zk-query-file-predicates
      `(("k" . ,#'org-zk-query-pred-keyword)
        ("c" . ,#'org-zk-query-pred-collection)
        ("s" . ,#'org-zk-query-pred-gtd-state)))

(provide 'org-zk-query)
