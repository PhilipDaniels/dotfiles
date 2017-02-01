;;; The pd layer is dependent upon these layers. Specifying the dependency here
;;; means that the user does not have to explicitly add them to
;;; dotspacemacs-configuration-layers.

(configuration-layer/declare-layer 'git)
(configuration-layer/declare-layer 'helm)
