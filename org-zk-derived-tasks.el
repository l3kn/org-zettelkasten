(defvar org-zk-derived-task-file "/home/leon/org/derived_tasks.org"
  "File to add derived tasks to")

(defvar org-zk-derived-tasks
  (make-hash-table :size 10 :test 'equal)
  "Hash-table of derived task hooks")

(defvar org-zk-derived-tasks-interval (* 30 60)
  "Update derived tasks every n seconds.")

(defclass org-zk-derived-task ()
  ((title :initarg :title)
   (priority :initarg :priority)
   (tags :initarg :tags :initform '())
   (predicate :initarg :predicate)))

(defmethod org-zk-derived-task-due-p ((task org-zk-derived-task))
  "Check if a derived task is due."
  (funcall (oref task predicate)))

(defmethod org-zk-derived-task-new-p ((task org-zk-derived-task) current)
  "Check if a derived task is not already active in `org-zk-derived-task-file'"
  (with-slots (title) task
    (not (--any
          (and (string= (oref it title) title)
               (eq (oref it todo-type) 'todo))
          current))))

(cl-defmacro def-org-zk-derived-task (title priority tags &rest body)
  "Helper macro for defining derived tasks."
  `(puthash
   ,title
   (make-instance
    'org-zk-derived-task
    :title ,title
    :priority ,priority
    :tags ,tags
    :predicate (lambda () ,@body))
   org-zk-derived-tasks))

(defun org-zk-derived-tasks-new ()
  "Generate a list of new derived tasks not already contained in
  the `org-zk-derived-task-file'."
  (let ((current (oref (org-zk-cache-get org-zk-derived-task-file) headlines)))
    (->> (hash-table-values org-zk-derived-tasks)
        (-filter 'org-zk-derived-task-due-p)
        (--filter (org-zk-derived-task-new-p it current)))))

(defun org-zk-derived-tasks-update ()
  "Add new tasks to the `org-zk-derived-task-file'."
  (-if-let (tasks (org-zk-derived-tasks-new))
      (org-zk-in-file org-zk-derived-task-file
        (save-excursion
          (goto-char (point-max))
          (dolist (task tasks)
            (with-slots (title priority tags) task
              (org-insert-heading nil nil t)
              (insert (format "NEXT [#%s] %s" priority title))
              (org-set-tags tags)))))))

(run-at-time nil org-zk-derived-tasks-interval 'org-zk-derived-tasks-update)

(provide 'org-zk-derived-tasks)
