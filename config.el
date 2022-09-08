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

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;; Set load path
(add-to-list 'load-path (expand-file-name "~/.config/crafted-emacs/lisp"))

;; Turn on visual line mode for text modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;;; Litering
(crafted-package-install-package 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;; User Interface
(require 'crafted-ui)

;; Show column position in mode-line
(column-number-mode t)

(customize-set-variable 'doom-modeline-buffer-file-name-style 'relative-from-project)

;; Install minions
(crafted-package-install-package 'minions)
(add-hook 'doom-modeline-mode-hook 'minions-mode)

;; Set config variables
(custom-set-variables '(crafted-ui-display-line-numbers t)
                      '(crafted-startup-inhibit-splash t))
;;; Evil mode
(require 'crafted-evil)        ; An `evil-mode` configuration

;; Set configuration variables
(custom-set-variables '(crafted-evil-discourage-arrow-keys t)
                      '(evil-want-C-u-scroll nil))

;; Set preferred key bindings
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-M-j") 'consult-buffer)

(define-key evil-motion-state-map (kbd "[ j") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "] j") 'evil-jump-forward)

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

;; Make it possible to ignore risky local variables
(advice-add 'risky-local-variable-p :override #'ignore)

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
(crafted-package-install-package 'lsp-treemacs)
(crafted-package-install-package 'consult-lsp)

;;; Clojure
;;; Structural editing
(crafted-package-install-package 'lispy)
(crafted-package-install-package 'lispyville)

(with-eval-after-load 'lispyville
  (lispyville-set-key-theme
   '(operators
     c-w
     slurp/barf-cp
     (additional-wrap normal visual insert)
     (escape insert)
     ;; need to resolve conflicts with unimpaired-like key bindings
     ;; (additional-movement normal visual motion)
     atom-movement)))

(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'lispy-mode-hook #'lispyville-mode)

;; Global defaults
(require 'eldoc)
(crafted-package-install-package 'aggressive-indent)
(crafted-package-install-package 'cider)
(crafted-package-install-package 'clojure-mode)
(require 'cljstyle-mode)

(setq clojure-align-forms-automatically t
      clojure-indent-style 'align-arguments)

;; (add-hook 'clojure-mode-hook    (lambda () (modify-syntax-entry ?- "w")))
;; (add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))

;; (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
;; (add-hook 'clojure-mode-hook #'cljstyle-mode)
(add-hook 'clojure-mode-hook #'lispy-mode)
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

;; (defun cider--prefer-lsp-xref-h ()
;;   (dolist (buffer (buffer-list))
;;     (with-current-buffer buffer
;;       (when (and (derived-mode-p 'clojure-mode)
;;                  (bound-and-true-p cider-use-xref)
;;                  (bound-and-true-p lsp-enable-xref))
;;         (remove-hook 'xref-backend-functions #'cider--xref-backend :local)
;;         (remove-hook 'xref-backend-functions #'lsp--xref-backend :local)
;;         (add-hook 'xref-backend-functions #'cider--xref-backend nil :local)
;;         (add-hook 'xref-backend-functions #'lsp--xref-backend nil :local)))))
;;
;; (add-hook 'cider-connected-hook #'cider--prefer-lsp-xref-h)

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

;;; Org mode
(require 'crafted-org)
(setq org-ellipsis " ▾")

;;; YAML
(crafted-package-install-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook 'lsp)

;;; config.el ends here
