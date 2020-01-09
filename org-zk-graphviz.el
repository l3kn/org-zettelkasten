(defclass org-zk-graphviz-graph ()
  ((name :initarg :name :initform "")
   (nodes :initarg :nodes :initform nil)
   (edges :initarg :edges :initform nil)))

(defclass org-zk-graphviz-node ()
  ((title :initarg :title :initform "")
   (id :initarg :id :initform nil)))

(defclass org-zk-graphviz-edge ()
  ((from :initarg :from)
   (to :initarg :to)))

(defun org-zk-graph-collect (path depth graph)
  "Collect connected nodes up to DEPTH"
  (message (format "%s @ %s" path depth))
  (let ((cache-file (org-el-cache-get path)))
    (when cache-file
      (add-node
       graph
       (make-instance
        'org-zk-graphviz-node
        :title (org-el-cache-get-keyword cache-file "TITLE")
        :id (oref cache-file path)))
      (when (plusp depth)
        (dolist (link (oref cache-file links))
          (let ((link-path (oref link path))
                (link-type (oref link type)))
            (when (and link-path (string-prefix-p "zk_" link-type))
              (add-edge
               graph
               (oref cache-file path)
               link-path)
            (org-zk-graph-collect link-path (1- depth) graph))))))))

;; 1. collect children of degree n
;; 2. connection matrix (upgrade edge types)
;; 3. draw edges
;; path as id
(defun org-zk-graph (path)
  (interactive (list (buffer-file-name)))
  (let* ((cache-file (org-el-cache-get path))
         (graph (make-instance 'org-zk-graphviz-graph :name "Graph")))
    (org-zk-graph-collect path 4 graph)
    (save-image graph "/home/leon/graph.png")))

(defun org-zk-graphviz-escape (id)
  "Escape filenames for use as graphviz node IDs"
  (replace-regexp-in-string (rx (any "/" "-" "_" "." "~")) "" id))

(defun org-zk-graphviz-show ()
  "Show graph for current buffer"
  (interactive)
  (let* ((path (buffer-file-name))
         (cache-file (org-el-cache-get path))
         (graph (make-instance 'org-zk-graphviz-graph :name "Graph")))
    (org-zk-graph-collect path 4 graph)
    (save-image graph "/home/leon/graph.png")
    (find-file "/home/leon/graph.png")
    (image-mode)))

(defmethod save-image ((graph org-zk-graphviz-graph) outfile)
  (interactive)
  (with-slots (name nodes edges) graph
    ;; (let* ((infile (make-temp-file "graph-editor" nil ".dot")))
    (let* ((infile "/home/leon/graph.dot"))
      (with-temp-file infile
        (progn
          (insert "graph ")
          ;; (insert name " ")
          (insert " {\n")
          (insert "  ratio=\"fill\";\n")
          (insert "  size=\"10,6!\";\n")
          (insert "  resolution=128;\n")
          (insert "  overlap=false;\n")
          (insert "  splines=true;\n")
          (insert "  node[fontsize=20];\n")

          (dolist (node nodes)
            (with-slots (id title) node
              (insert (format "%s [ label=\"%s\" ];\n"
                              (org-zk-graphviz-escape id) title))))
          (dolist (edge edges)
            (if (< 0.5 (cl-random 1.0))
                (insert (format "%s--%s;\n"
                                (org-zk-graphviz-escape (oref edge from))
                                (org-zk-graphviz-escape (oref edge to))))
              (insert (format "%s--%s;\n"
                                (org-zk-graphviz-escape (oref edge to))
                                (org-zk-graphviz-escape (oref edge from)))))
            )
          (insert "}\n")))
      ;(shell-command (format "dot %s -Tpng -Kfdp -o %s" infile outfile))
      (shell-command (format "dot %s -Tpng -o %s" infile outfile)))))

(defmethod add-edge ((graph org-zk-graphviz-graph) from to)
  (with-slots (edges nodes) graph
    (let ((edge (if (string< from to)
                    (make-instance 'org-zk-graphviz-edge :from from :to to)
                  (make-instance 'org-zk-graphviz-edge :from to :to from))))
      (setq edges (remove-duplicates (cons edge edges)
                                     :test 'equalp)))))


(defmethod add-node ((graph org-zk-graphviz-graph) node)
  (with-slots (nodes) graph
    (setq nodes (remove-duplicates (cons node nodes)))))

;; (let ((graph (make-instance 'org-zk-graphviz-graph :name "test")))
;;   (add-edge graph 1 4)
;;   (add-edge graph 1 2)
;;   (add-edge graph 1 7)
;;   (add-edge graph 7 8)
;;   (add-edge graph 8 9)
;;   (add-edge graph 8 1)
;;   (save-image graph "~/test.png"))


;; (defun ge-find-node-id-str (id-str)
;;   (--find
;;    (string= (ge-translate-id (oref it id)) id-str)
;;    (oref ge-cur-graph nodes)))

;; (defun ge-translate-id (id)
;;   (format "%c%c"
;;    (+ ?a (/ id 26))
;;    (+ ?a (% id 26))))

;; (defun ge-select-node ()
;;   (ge-find-node-id-str
;;    (format "%c%c"
;;            (read-char)
;;            (read-char))))

;; (defun ge-add-edge ()
;;   (interactive)
;;   (add-edge ge-cur-graph (ge-select-node) (ge-select-node))
;;   (to-image))

;; (defun ge-add-node ()
;;   (interactive)
;;   (add-node ge-cur-graph (read-string "Name>"))
;;   (to-image))

;; (defun ge-clear ()
;;   (interactive)
;;   (setq ge-cur-graph (make-instance 'ge-graph))
;;   (to-image))

;; (defun ge-save ()
;;   (interactive)
;;   (with-current-buffer (find-file-noselect "~/graph-editor.lisp")
;;     (erase-buffer)
;;     (insert (prin1-to-string ge-cur-graph))
;;     (save-buffer)))

;; (setq ge-cur-graph (make-instance 'ge-graph))

;; (define-derived-mode graph-editor-mode image-mode "Graph Editor"
;;   "Mode for editing graphs")

;; (setq graph-editor-mode-map
;;       (let ((map (make-sparse-keymap)))
;;         (define-key map (kbd "e") #'ge-add-edge)
;;         (define-key map (kbd "n") #'ge-add-node)
;;         (define-key map (kbd "s") #'ge-save)
;;         (define-key map (kbd "g") #'to-image)
;;         map))


;; ;; TODO: Buffer-local variables

;; (defclass org-zk-graph ()
;;   ((nodes :initform nil)
;;    (edges :initform nil)))

;; (defclass org-zk-node ()
;;   ((path :initarg :path)
;;    (label :initarg :label)
;;    (depth :initarg :depth)))

;; (defun make-org-zk-graph ()
;;   (make-instance 'org-zk-graph))

;; (defun org-zk-edge-equal-p (a b)
;;   (or (and (string= (car a) (car b))
;;            (string= (cadr a) (cadr b)))
;;       (and (string= (car a) (cadr b))
;;            (string= (cadr a) (car b)))))

;; ;; from & to are file path
;; (defmethod add-edge ((graph org-zk-graph) from to type)
;;   (if (and (string-suffix-p ".org" from)
;;            (string-suffix-p ".org" to))
;;       (with-slots (nodes edges) graph
;;         (let ((edge (if (eq type 'child)
;;                         (list to from 'parent)
;;                       (list to from type))))
;;           (unless (--any (org-zk-edge-equal-p it edge) edges)
;;             (push edge edges))))))

;; (defmethod add-node ((graph org-zk-graph) path label depth)
;;   (with-slots (nodes edges) graph
;;     (-if-let (e (--find (string= (oref it path) path)
;;                         nodes))
;;         (oset e depth (min (oref e depth) depth))
;;       (push (make-instance 'org-zk-node :path path :label label :depth depth) nodes))))


;; (defun org-zk-graphviz-buffer ()
;;   (interactive)
;;   (org-zk-graphviz (org-zk-link-graph (buffer-file-name) 6)))

;; (defmethod org-zk-graphviz ((graph org-zk-graph))
;;   (let* ((inf "~/.emacs.d/graph-editor.dot")
;;          (outf "~/.emacs.d/graph-editor.png"))
;;     (with-temp-file inf
;;       (progn
;;         (insert "graph ")
;;         (insert "test" " ")
;;         (insert " {\n")
;;         (insert "overlap=false;\n")
;;         (insert "splines=true;\n")
;;         (insert "nodesep=0.5;\n")
;;         (dolist (node (oref graph nodes))
;;           (insert (format "\"%s\" [ label= \"%s\"; fontsize=%d ];\n"
;;                           (oref node path)
;;                           (oref node label)
;;                           (nth (oref node depth) '(30 25 22 20 16 14 14 14))
;;                           )))
;;         (dolist (edge (oref graph edges))
;;           (if (> (cl-random 1.0) 0.5)
;;               (insert (format "\"%s\"--\"%s\";\n"
;;                               (car edge)
;;                               (cadr edge)))
;;             (insert (format "\"%s\"--\"%s\";\n"
;;                             (cadr edge)
;;                             (car edge))))
;;           ;; (if (eq (caddr edge) 'parent)
;;           ;;     (insert (format "\"%s\"--\"%s\";\n"
;;           ;;                     (car edge)
;;           ;;                     (cadr edge)))
;;           ;;   (insert (format "\"%s\"--\"%s\";\n"
;;           ;;                   (car edge)
;;           ;;                   (cadr edge))))
;;           )
;;         ;; TODO: Handle file type
;;         (insert "}\n")))
;;     (shell-command (format "dot %s -Tpng -o %s" inf outf))))

;; (defun org-zk-link-graph (file depth)
;;   (let ((graph (make-org-zk-graph)))
;;     (org-zk--link-graph graph file 0 depth)
;;     graph))

;; (defun org-zk--link-graph (graph file depth max-depth)
;;   (if (string-suffix-p ".org" file)
;;       (add-node graph file (org-zk-file-title file) depth))
;;   (if (< depth max-depth)
;;       (-when-let (entry (org-el-cache-get file))
;;         (--each
;;             (oref entry links)
;;           (let ((target (oref it path)))
;;             (pcase (oref it type)
;;               ;; Don't include siblings from other parents
;;               ("zk_parent"
;;                (add-edge graph file target 'parent)
;;                ;; (unless (> depth 0)
;;                (org-zk--link-graph graph target (1+ depth) max-depth))
;;               ;; )
;;               ("zk_child"
;;                (add-edge graph file target 'child)
;;                (org-zk--link-graph graph target (1+ depth) max-depth)
;;                )
;;               ("zk_friend"
;;                (add-edge graph file target 'friend)
;;                (org-zk--link-graph graph target (1+ depth) max-depth))))))))
