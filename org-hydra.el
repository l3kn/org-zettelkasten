(require 'hydra)

(defhydra zettelkasten-hydra (:hint none)
  "
^GTD^           ^Add Edge^   ^Create Edge^
^^^^^----------------------------------------------
_g_: Set State  _p_: Parent  _P_: Parent
^ ^             _c_: Child   _C_: Child
^ ^             _f_: Friend  _F_: Friend
^ ^             _o_: Other   _O_: Other
"
  ("g" org-projects-set-gtd-state "Set GTD State" :exit t)
  ("p" zk-add-parent "Add parent")
  ("c" zk-add-child "Add child")
  ("f" zk-add-friend "Add friend")
  ("o" zk-add-edge "Add other edge")
  ("P" zk-create-parent "Create parent")
  ("C" zk-create-child "Create child")
  ("F" zk-create-friend "Create friend")
  ("O" zk-create-edge "Create other edge"))

(provide 'org-hydra)
