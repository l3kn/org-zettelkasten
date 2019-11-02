(require 'hydra)

(defhydra org-zk-hydra (:hint none)
  "
^GTD^           ^Add Edge^   ^Create Edge^
^^^^^----------------------------------------------
_g_: Set State  _p_: Parent  _P_: Parent
^ ^             _c_: Child   _C_: Child
^ ^             _f_: Friend  _F_: Friend
^ ^             _o_: Other   _O_: Other
"
  ("g" org-zk-projects-set-gtd-state "Set GTD State" :exit t)
  ("p" org-zk-add-parent "Add parent")
  ("c" org-zk-add-child "Add child")
  ("f" org-zk-add-friend "Add friend")
  ("o" org-zk-add-edge "Add other edge")
  ("P" org-zk-create-parent "Create parent")
  ("C" org-zk-create-child "Create child")
  ("F" org-zk-create-friend "Create friend")
  ("O" org-zk-create-edge "Create other edge"))

(provide 'org-zk-hydra)
