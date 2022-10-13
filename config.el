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
                      '(evil-want-minibuffer nil)
                      '(evil-want-C-u-scroll nil))

;; Set preferred key bindings
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-M-j") 'consult-buffer)

(define-key evil-motion-state-map (kbd "[ j") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "] j") 'evil-jump-forward)

(define-key evil-window-map (kbd "C-q") 'evil-quit)

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

;; Returns the parent directory containing a .project.el file, if any,
;; to override the standard project.el detection logic when needed.
(defun pv-project-override (dir)
  (let ((override (locate-dominating-file dir ".project.el")))
    (if override
      (cons 'vc override)
      nil)))

(with-eval-after-load 'project
  (add-hook 'project-find-functions #'pv-project-override))

;;; Buffer editing
(require 'crafted-editing)     ; Whitspace trimming, auto parens etc.

;;; Language server protocol
(crafted-package-install-package 'lsp-mode)

(setq lsp-eldoc-enable-hover nil
      lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
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

;;; Lisp family languages config
(require 'crafted-lisp)

;;; Structural editing
(crafted-package-install-package 'smartparens)
(require 'smartparens-config)

(crafted-package-install-package 'evil-smartparens)

(custom-set-variables
 '(show-smartparens-global-mode t)
 '(sp-navigate-interactive-always-progress-point t))

(defun pv-smartparens-setup ()
 (define-key smartparens-mode-map (kbd "M-s u") 'sp-splice-sexp)
 (define-key smartparens-mode-map (kbd "M-s r") 'sp-raise-sexp)
 (define-key smartparens-mode-map (kbd "M-s >") 'sp-forward-barf-sexp)
 (define-key smartparens-mode-map (kbd "M-s <") 'sp-forward-slurp-sexp)
 (define-key smartparens-mode-map (kbd "M-s w (") 'sp-wrap-round)
 (define-key smartparens-mode-map (kbd "M-s w [") 'sp-wrap-square)
 (define-key smartparens-mode-map (kbd "M-s w {") 'sp-wrap-curly))

(add-hook 'smartparens-mode-hook #'pv-smartparens-setup)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

;;; Clojure
(require 'cljstyle-mode)

;; cider-defun-at-point treats contents of comment block as top level functions
(customize-set-variable 'clojure-toplevel-inside-comment-form t)

(setq clojure-align-forms-automatically t
      clojure-indent-style 'align-arguments)

(add-hook 'clojure-mode-hook #'evil-smartparens-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook 'lsp)

;; accept kebab-case words
(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?- "w")))

;;; Cider
(defun pv-cider-format-buffer ()
  (interactive)
  (and (bound-and-true-p cider-mode) (cider-format-buffer)))

;; set default completion style for cider
;; this should help improve completion related performance
(setq completion-category-defaults '((cider (styles basic))))

(add-hook 'cider-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        `(,(cape-capf-properties #'cider-complete-at-point :exclusive 'no)
                          cape-dabbrev
                          cape-file))))

;;; Org mode
(require 'crafted-org)
(setq org-ellipsis " ▾")

;;; Http Client
(crafted-package-install-package 'verb)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;;; YAML
(crafted-package-install-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook 'lsp)

;;; config.el ends here
