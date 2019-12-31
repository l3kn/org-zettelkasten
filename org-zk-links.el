(defvar org-zk-links-follow nil
  "Switch to target file after adding a link.")

(defun org-zk-links-toggle-follow ()
  "Toggle link-following on creation."
  (interactive)
  (if org-zk-links-follow
      (progn
        (setq org-zk-links-follow nil)
        (message "org-zk link follow disabled"))
    (progn
        (setq org-zk-links-follow t)
        (message "org-zk link follow enabled"))))

(defun org-zk-add-link (type target)
  "Add a link of TYPE to TARGET to the current buffer's link list."
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
             (org-zk-file-title target)))
    ;; For the first link, insert a newline between it an the main text
    (forward-line)
    (if (looking-at-p (rx alpha))
        (insert "\n"))))

(defun org-zk-add-plain-link (target title)
  "Add a 'plain' link to the current buffer's link list."
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
    (insert (format "- [[%s][%s]]\n" target title))
    ;; For the first link, insert a newline between it an the main text
    (forward-line)
    (if (looking-at-p (rx alpha))
        (insert "\n"))))

;; TODO: Allow custom link prefix
;; TODO: Allow custom link types
(defun org-zk-change-link-type (new-type target)
  "Change the type of all links to TARGET in the current buffer
to NEW-TYPE. This changes inline-links, too."
  (replace-regexp
   (concat
    (rx
     "[[zk_" (or "parent" "child" "friend") ":")
    (regexp-quote target)
    (rx "]"))
   (format "[[zk_%s:%s]" new-type target)
   nil (point-min) (point-max)))

(defvar org-zk-link-re
  (rx
   "[[zk_"
   (group-n 1 (or "parent" "child" "friend"))
   ":"
   (group-n 2 (* (not (any "]"))))
   "]["
   (group-n 3 (* (not (any "]"))))
   "]]")
  "Regex matching zettelkasten links.
Groups:
1. type
2. target
3. text / title")

(defvar org-zk-list-link-re
  (rx
   "-"
   (+ (any " " "\t"))
   "[[zk_"
   (group-n 1 (or "parent" "child" "friend"))
   ":"
   (group-n 2 (* (not (any "]"))))
   "]["
   (group-n 3 (* (not (any "]"))))
   "]]")
  "Regex matching zettelkasten link list entries.
Groups:
1. type
2. target
3. text / title")

(defvar org-zk-deleted-link-text "(deleted)")

;; TODO: Jump to file for manual fixes
;;
;; NOTE: It's easier to parse the buffer again instead of adding link bounds
;; to the org cache and dealing with cache-incoherence problems.
(defun org-zk-remove-inline-link (target)
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

(defun org-zk-remove-link (target)
  "Remove the link to TARGET from the link list in the current buffer."
  (save-excursion
    (goto-char (point-min))
    ;; skip keywords
    (while (looking-at-p "^#+")
      (forward-line))
    ;; skip whitespace
    (while (looking-at-p "^\w*$")
      (forward-line))
    ;; loop over link list
    (while (looking-at org-zk-list-link-re)
      (if (string= (match-string 2) target)
          (kill-line)
        (forward-line)))))

(defun org-zk-buffer-edges ()
  "Return a list (type target title) for all edges in the current buffer."
  (let (res)
    (save-excursion
      (goto-char (point-min))
      ;; skip keywords
      (while (looking-at-p "^#+")
        (forward-line))
      ;; skip whitespace
      (while (looking-at-p "^\w*$")
        (forward-line))
      ;; loop over link list
      (while (looking-at org-zk-list-link-re)
        (push (list (match-string-no-properties 1)
                    (match-string-no-properties 2)
                    (match-string-no-properties 3))
              res)
          (forward-line)))
    res))


(defun org-zk-select-edge (action)
  "Call FN with the target of an edge in the current buffer."
  (ivy-read
   "Edge: "
   (mapcar
    (lambda (e)
      (cons
       (format "%s (%s)" (third e) (first e))
       (second e)))
    (org-zk-buffer-edges))
   :action action))

(defun org-zk--remove-edge (target)
  (let ((source (buffer-file-name)))
    (org-zk-remove-link target)
    (save-buffer)
    (if (file-exists-p target)
        (org-zk-in-file target
          (org-zk-remove-link source)
          (save-buffer)))))

(defun org-zk-remove-edge ()
  "Select and remove an edge of the current buffer."
  (interactive)
  (org-zk-select-edge
   (lambda (selection)
     (org-zk--remove-edge (cdr selection)))))

(defun org-zk-edge-inverse (type)
  (pcase type
    ("parent" "child")
    ("child" "parent")
    ("friend" "friend")))

(defun org-zk--add-edge (type target)
  (let ((inverse (org-zk-edge-inverse type))
        (source (buffer-file-name)))
    (org-zk-add-link type target)
    (save-buffer)
    (when inverse
      (if org-zk-links-follow
          (with-current-buffer (find-file target)
            (org-zk-add-link inverse source))
        (org-zk-in-file target
            (org-zk-add-link inverse source))))))

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

(defun org-zk--try-yank-url ()
  "If the result of `yank' is a url,
return it, if not, return NIL."
  )

;; TODO: Get title from url
(defun org-zk-add-yank-link ()
  "Check if the element in the clipboard / kill-ring is an URL,
if so, insert a link to it in the edge list, prompting for a description."
  (interactive)
  (let ((res
         (with-temp-buffer
           (yank)
           (thing-at-point 'url (point)))))
    (if res
        (org-zk-add-plain-link res (read-string "Description: "))
      (message "Kill-ring contents are not a URL"))))

(provide 'org-zk-links)
