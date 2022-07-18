;;; config.el -- Example Crafted Emacs user customization file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Crafted Emacs supports user customization through a `config.el' file
;; similar to this one.  You can copy this file as `config.el' to your
;; Crafted Emacs configuration directory as an example.
;;
;; In your configuration you can set any Emacs configuration variable, face
;; attributes, themes, etc as you normally would.

;;; Core settings

(require 'crafted-defaults)    ; Sensible default settings for Emacs

;;; Litering
(crafted-package-install-package 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;; User Interface
(require 'crafted-ui)

;; Install minions
(crafted-package-install-package 'minions)
(add-hook 'doom-modeline-mode-hook 'minions-mode)

;; Set config variables
(custom-set-variables '(crafted-ui-display-line-numbers t))

;;; Evil mode
(require 'crafted-evil)        ; An `evil-mode` configuration

;; Set configuration variables
(custom-set-variables '(crafted-evil-discourage-arrow-keys t)
                      '(evil-want-C-u-scroll t))

;; Set preferred key bindings
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-M-j") 'consult-buffer)

;;; Completions and Actions
(require 'crafted-completion)  ; selection framework based on `vertico`

(define-key minibuffer-local-map (kbd "C-d") 'embark-act)
(define-key project-prefix-map (kbd "g") 'consult-ripgrep)

;;; Source Control
(crafted-package-install-package 'magit)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
(global-set-key (kbd "C-M-;") #'magit-status)

(crafted-package-install-package 'magit-delta)
(add-hook 'magit-mode-hook 'magit-delta-mode)

(crafted-package-install-package 'git-timemachine)

;;; Project Management
(require 'crafted-project)

(define-key project-prefix-map (kbd "g") 'consult-ripgrep)

;;; Buffer editing
(require 'crafted-editing)     ; Whitspace trimming, auto parens etc.

;;; Language server protocol
(crafted-package-install-package 'lsp-mode)

(setq lsp-eldoc-enable-hover t
      lsp-signature-auto-activate nil
      lsp-headerline-breadcrumb-enable nil
      lsp-lens-enable t)


(crafted-package-install-package 'lsp-ui)

(custom-set-variables
  '(lsp-ui-doc-enable t)
  '(lsp-ui-doc-show-with-mouse nil)
  '(lsp-ui-doc-show-with-cursor nil)
  '(lsp-ui-doc-childframe nil))

(crafted-package-install-package 'flycheck)

;;; Clojure

;; Global defaults
(require 'eldoc)
;; keeps code indented even when copy/pasting.
(crafted-package-install-package 'aggressive-indent)

(crafted-package-install-package 'cider)

;; add cider xref backend with lower priority than lsp when connected
(defvar-local cider-lsp-xref-fns
    (mapcar (lambda (fn)
              `(lambda ()
                 (and (fboundp ',fn)
                      (funcall ',fn))))
            `(cider--xref-backend
              lsp--xref-backend)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (mapc (lambda (fn)
                    (add-hook 'xref-backend-functions fn nil t))
                  cider-lsp-xref-fns)))
(crafted-package-install-package 'clojure-mode)

(add-hook 'clojure-mode #'aggressive-indent-mode)
(add-hook 'clojure-mode (lambda () (modify-syntax-entry ?- "w")))
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

;;; config.el ends here
