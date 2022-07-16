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

;; Setup motus themes
;; Add all your customizations prior to loading the themes
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-region '(bg-only))
(load-theme 'modus-vivendi t)

;;; Evil mode
(require 'rational-evil)        ; An `evil-mode` configuration

;;; Completions and Actions
(require 'rational-completion)  ; selection framework based on `vertico`

;;; Source Control
(rational-package-install-package 'magit)

;;; Project Management
(require 'rational-project)

;; (require 'rational-use-package) ; Configuration for `use-package`
(require 'rational-editing)     ; Whitspace trimming, auto parens etc.
(require 'rational-compile)     ; automatically compile some emacs lisp files

;; ;;; config.el ends here
