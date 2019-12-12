(defun org-zk-extract-heading ()
  "Extract the current subheading to a zettel, replace it with a
  link to that zettel."
  ;; TODO
  )

(defclass org-zk-graph ()
  ((nodes :initform nil)
   (edges :initform nil)))

(defclass org-zk-node ()
  ((path :initarg :path)
   (label :initarg :label)
   (depth :initarg :depth)))

(defun make-org-zk-graph ()
  (make-instance 'org-zk-graph))

(defun org-zk-edge-equal-p (a b)
  (or (and (string= (car a) (car b))
           (string= (cadr a) (cadr b)))
      (and (string= (car a) (cadr b))
           (string= (cadr a) (car b)))))

;; from & to are file path
(defmethod add-edge ((graph org-zk-graph) from to type)
  (if (and (string-suffix-p ".org" from)
           (string-suffix-p ".org" to))
      (with-slots (nodes edges) graph
        (let ((edge (if (eq type 'child)
                        (list to from 'parent)
                      (list to from type))))
          (unless (--any (org-zk-edge-equal-p it edge) edges)
            (push edge edges))))))

(defmethod add-node ((graph org-zk-graph) path label depth)
  (with-slots (nodes edges) graph
    (-if-let (e (--find (string= (oref it path) path)
                        nodes))
        (oset e depth (min (oref e depth) depth))
      (push (make-instance 'org-zk-node :path path :label label :depth depth) nodes))))


(defun org-zk-graphviz-buffer ()
  (interactive)
  (org-zk-graphviz (org-zk-link-graph (buffer-file-name) 6)))

(defmethod org-zk-graphviz ((graph org-zk-graph))
  (let* ((inf "~/.emacs.d/graph-editor.dot")
         (outf "~/.emacs.d/graph-editor.png"))
    (with-temp-file inf
      (progn
        (insert "graph ")
        (insert "test" " ")
        (insert " {\n")
        (insert "overlap=false;\n")
        (insert "splines=true;\n")
        (insert "nodesep=0.5;\n")
        (dolist (node (oref graph nodes))
          (insert (format "\"%s\" [ label= \"%s\"; fontsize=%d ];\n"
                          (oref node path)
                          (oref node label)
                          (nth (oref node depth) '(30 25 22 20 16 14 14 14))
                          )))
        (dolist (edge (oref graph edges))
          (if (> (cl-random 1.0) 0.5)
              (insert (format "\"%s\"--\"%s\";\n"
                              (car edge)
                              (cadr edge)))
            (insert (format "\"%s\"--\"%s\";\n"
                            (cadr edge)
                            (car edge))))
          ;; (if (eq (caddr edge) 'parent)
          ;;     (insert (format "\"%s\"--\"%s\";\n"
          ;;                     (car edge)
          ;;                     (cadr edge)))
          ;;   (insert (format "\"%s\"--\"%s\";\n"
          ;;                   (car edge)
          ;;                   (cadr edge))))
          )
        ;; TODO: Handle file type
        (insert "}\n")))
    (shell-command (format "dot %s -Tpng -o %s" inf outf))))

(defun org-zk-link-graph (file depth)
  (let ((graph (make-org-zk-graph)))
    (org-zk--link-graph graph file 0 depth)
    graph))

(defun org-zk--link-graph (graph file depth max-depth)
  (if (string-suffix-p ".org" file)
      (add-node graph file (org-zk-file-title file) depth))
  (if (< depth max-depth)
      (-when-let (entry (org-zk-cache-get file))
        (--each
            (oref entry links)
          (let ((target (oref it path)))
            (pcase (oref it type)
              ;; Don't include siblings from other parents
              ("zk_parent"
               (add-edge graph file target 'parent)
               ;; (unless (> depth 0)
               (org-zk--link-graph graph target (1+ depth) max-depth))
              ;; )
              ("zk_child"
               (add-edge graph file target 'child)
               (org-zk--link-graph graph target (1+ depth) max-depth)
               )
              ("zk_friend"
               (add-edge graph file target 'friend)
               (org-zk--link-graph graph target (1+ depth) max-depth))))))))


(defun org-zk-files-linking-here ()
  "Generate a list of files linking to the current buffer."
  (let ((path (buffer-file-name)))
    (org-zk-cache-filter
     (lambda (source file)
       (--any (string= (oref it path) path)
              (oref file links))))))

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

(defun org-zk-add-edge-to-buffer (type target)
  ;; Skip keywords
  (save-excursion
    (goto-char (point-min))
    (while (looking-at-p "^#+")
      (forward-line))
    (if (looking-at-p "^$")
        (forward-line)
      (insert "\n"))
    ;; TODO: Skip lines of "lesser / sorted prev" type
    (insert (format
             "- [[zk_%s:%s][%s]]\n"
             type
             target
             (org-zk-file-title target)))
    (save-buffer)))

(defun org-zk--add-edge-to-buffer (type target)
  (let ((edges (or (org-zk-buffer-edges)
                   (org-zk-edges-insert-block)))
        (source (buffer-file-name)))
    (zk-add-link type )
    (save-buffer)))

(defun org-zk-edge-inverse (type)
  (pcase type
    ("parent" "child")
    ("child" "parent")
    ("friend" "friend")))

(defun org-zk--add-edge (type target)
  (org-zk-add-edge-to-buffer type target)
  (let ((inverse (org-zk-edge-inverse type))
        (source (buffer-file-name)))
    (when inverse
      (with-current-buffer (find-file-noselect target)
        (org-zk-add-edge-to-buffer inverse source)))))

(defun org-zk-add-edge (type)
  (org-zk-select-file
   (lambda (selection)
     (org-zk--add-edge type (cdr selection)))))

(defun org-zk-create-edge (edge-type title)
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
  (org-zk-add-edge "parent"))

(defun org-zk-add-child ()
  (interactive)
  (org-zk-add-edge "child"))

(defun org-zk-add-friend ()
  (interactive)
  (org-zk-add-edge "friend"))

(defun org-zk-create-parent (title)
  (interactive (list (org-zk-read-title)))
  (org-zk-create-edge "parent" title))

(defun org-zk-create-child (title)
  (interactive (list (org-zk-read-title)))
  (org-zk-create-edge "child" title))

(defun org-zk-create-friend (title)
  (interactive (list (org-zk-read-title)))
  (org-zk-create-edge "friend" title))

(defun org-zk-file-title (file)
  (let ((cache-file (org-zk-cache-get (expand-file-name file))))
    (if cache-file
        (org-zk-cache-get-keyword cache-file "TITLE")
      file)))

(defun org-zk-make-link (file &optional title)
  (let ((category (org-zk-category-for-file file)))
    (if category
        (funcall
         (org-zk-category-link-fn category)
         file
         (or title (org-zk-file-title file)))
      (error "File %s is not part of any zettelkasten category" file))))

(provide 'org-zk-links)
