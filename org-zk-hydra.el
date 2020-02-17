(require 'hydra)
(require 'org-zk-keywords)

(defhydra org-zk-hydra (:columns 4)
  ("g" org-zk-set-gtd-state "Set GTD State")
  ("t" org-zk-set-category "Set Category / Type")
  ("s" org-zk-set-stability "Stability")
  ("f" ace-link-org "Open Link")
  ("l" org-zk-link-file "Link File")
  ("h" avy-org-goto-heading-timer "Jump Heading")
  ("o" org-zk-open-file "Open File")
  ("R" org-zk-rename "Rename")
  ("k" org-zk-add-keyword "Add Keyword")
  ("K" org-zk-edit-keywords "Add Keyword")
  ;; I'm using C-b as a hotkey for the hydra
  ;; double-tapping b opens a file
  ("b" org-zk-open-file "Open File")
  ("C-b" org-zk-open-file "Open File")
  ("L" org-zk-backlinks "Show backlinks")
  ("F" org-zk-file-view "File View" :exit t))

(provide 'org-zk-hydra)
