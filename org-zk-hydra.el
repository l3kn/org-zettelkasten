(require 'hydra)
(require 'org-zk-keywords)
(require 'org-zk-links)

(defhydra org-zk-hydra (:hint none)
  "
^Actions^        ^Keywords^      ^Edges^
^^^^^--------------------------------------------
_l_: Open Link   _g_: State      _p_: + Parent
_o_: Open File   _,_: Priority   _c_: + Child
_n_: New File    _t_: Type       _f_: + Friend
_h_: Jump Hdng   _s_: Stability
_R_: Rename
_L_: Show Backlinks
"
  ("g" org-zk-set-gtd-state "Set GTD State")
  ("t" org-zk-set-category "Set Category / Type")
  ("," org-zk-set-gtd-priority "Priority")
  ("s" org-zk-set-stability "Stability")
  ("l" ace-link-org "Open Link")
  ("C-l" org-zk-link-file "Link File")
  ("h" avy-org-goto-heading-timer "Jump Heading")
  ("o" org-zk-open-file "Open File")
  ("n" org-zk-new-file "New File")
  ("R" org-zk-rename "Rename")
  ;; I'm using C-b as a hotkey for the hydra
  ;; double-tapping b opens a file
  ("b" org-zk-open-file "Open File")
  ("C-b" org-zk-open-file "Open File")
  ("p" org-zk-link-parent "Link parent")
  ("c" org-zk-link-child "Link child")
  ("f" org-zk-link-friend "Link friend")
  ("L" org-zk-backlinks "Show backlinks"))

(provide 'org-zk-hydra)
