(require 'hydra)
(require 'org-zk-keywords)
(require 'org-zk-links)

(defun org-zk--hydra-follow ()
  (if org-zk-links-follow
      "[X]"
      "[ ]"))

(defhydra org-zk-hydra (:hint none)
  "
^Actions^        ^Keywords^      ^Edges^        ^Options^
^^^^^----------------------------------------------------
_l_: Open Link   _g_: State      _p_: + Parent  _C-f_: %s(org-zk--hydra-follow) follow
_o_: Open File   _,_: Priority   _c_: + Child
_n_: New File    _t_: Type       _f_: + Friend
_h_: Jump Hdng   _s_: Stability  _y_: + Yank URL
_L_: Link File   ^ ^             _r_: Remove
_x_: Query
_R_: Rename
"
  ("g" org-zk-set-gtd-state "Set GTD State")
  ("t" org-zk-set-category "Set Category / Type")
  ("," org-zk-set-gtd-priority "Priority")
  ("s" org-zk-set-stability "Stability")
  ("l" ace-link-org "Open Link")
  ("L" org-zk-link-file "Link File")
  ("C-l" org-zk-link-file "Link File")
  ("h" avy-org-goto-heading-timer "Jump Heading")
  ("o" org-zk-open-file "Open File")
  ("n" org-zk-new-file "New File")
  ("x" org-zk-xapian-query-new-query "Xapian Query")
  ("R" org-zk-rename "Rename")
  ;; I'm using C-b as a hotkey for the hydra
  ;; double-tapping b opens a file
  ("b" org-zk-open-file "Open File")
  ("C-b" org-zk-open-file "Open File")
  ("p" org-zk-add-parent "Add parent")
  ("c" org-zk-add-child "Add child")
  ("f" org-zk-add-friend "Add friend")
  ("r" org-zk-remove-edge "Remove edge")
  ("y" org-zk-add-yank-link "Add yank link")
  ("C-f" org-zk-links-toggle-follow "Toggle follow"))

(provide 'org-zk-hydra)
