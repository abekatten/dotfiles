;; Initialisation of org-mode
;;
;;

(setq org-src-fontify-natively t        ;; Syntax highlighting in source blocks
;;      org-hide-emphasis-markers t     ;; Self-explanatory
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil)


;; This package makes headline bullets prettier
(use-package org-bullets
  :ensure t
  :init
  ;; Add as a hook. Howardism.org.
  (add-hook 'org-mode-hook 'org-bullets-mode))


;; Regular expression that matches dash and asterisk when they are the first
;; non-blank character on a line, ei. lists. Stolen from howardism.org.
;; They are substituted by a unicode bullet.
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


;; This macro looks for and set the first font that is installed on the system.
;; From howardism.org
;; It also changes the faces of headlines to be bigger and have normal color.
(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
  (load-theme 'gruvbox)
  (custom-theme-set-faces `gruvbox
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                         ;; `(org-level-1 ((t (,@variable-tuple :height 1.75 :weight bold :inherit default))))
;;                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
  
;; Keybindings
;; org-store-link
;; org-capture
;; indent-rigidly
;; org-end-of-item-list
;; org-beginning-of-item-list
;; outline-up-heading
;; org-table-copy-region
;; org-table-paste-rectangle

(provide 'init-org)
