;;; config.el -- Example Rational Emacs user customization file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Rational Emacs supports user customization through a `config.el' file
;; similar to this one.  You can copy this file as `config.el' to your
;; Rational Emacs configuration directory as an example.
;;
;; In your configuration you can set any Emacs configuration variable, face
;; attributes, themes, etc as you normally would.

;;; Core settings

(require 'rational-defaults)    ; Sensible default settings for Emacs

;;; User Interface
(require 'rational-ui)

;; Install minions
(rational-package-install-package 'minions)
(add-hook 'doom-modeline-mode-hook 'minions-mode)

;; Set config variables
(custom-set-variables '(rational-ui-display-line-numbers t))

;;; Evil mode
(require 'rational-evil)        ; An `evil-mode` configuration

;; Set configuration variables
(custom-set-variables '(rational-evil-discourage-arrow-keys t)
                      '(evil-want-C-u-scroll t))

;; Set preferred key bindings
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-M-u") 'universal-argument)

;;; Completions and Actions
(require 'rational-completion)  ; selection framework based on `vertico`

;;; Source Control
(rational-package-install-package 'magit)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
(global-set-key (kbd "C-M-;") #'magit-status)

;;; Project Management
(require 'rational-project)

;; (require 'rational-use-package) ; Configuration for `use-package`
(require 'rational-editing)     ; Whitspace trimming, auto parens etc.

;; ;;; config.el ends here
