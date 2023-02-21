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

;; highlight current line
(global-hl-line-mode t)

(customize-set-variable 'doom-modeline-buffer-file-name-style 'relative-from-project)

;; Install minions
(crafted-package-install-package 'minions)
(add-hook 'doom-modeline-mode-hook 'minions-mode)

;; Set config variables
(custom-set-variables '(crafted-ui-display-line-numbers t)
                      '(crafted-ui-use-doom-modeline t)
                      '(crafted-startup-inhibit-splash t))

(setq initial-buffer-choice #'recentf-open-files)


;;; Evil mode
(require 'crafted-evil)        ; An `evil-mode` configuration

;; Set configuration variables
(custom-set-variables '(crafted-evil-discourage-arrow-keys t)
                      '(evil-want-minibuffer nil)
                      '(evil-want-C-u-scroll nil))

(evil-set-initial-state 'Info-mode 'emacs)

;; Set preferred key bindings
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-M-j") 'consult-buffer)
(global-set-key (kbd "C-<return>") 'embark-act)

(define-key evil-motion-state-map (kbd "[ j") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "] j") 'evil-jump-forward)

(define-key evil-window-map (kbd "C-q") 'evil-quit)
(define-key evil-window-map (kbd "C-d") 'dired-other-window)
(define-key evil-window-map (kbd "C-M-j") 'consult-buffer-other-window)

;;; Completions and Actions
(require 'crafted-completion)  ; selection framework based on `vertico`

(customize-set-variable 'corfu-auto-delay 0.2) ; Set delay for automatic completion

(define-key minibuffer-local-map (kbd "C-d") 'embark-act)
(define-key project-prefix-map (kbd "g") 'consult-ripgrep)

;; use TAB in minibuffer to switch to embark actions and back
;; by https://karthinks.com/software/fifteen-ways-to-use-embark/
(defun with-minibuffer-keymap (keymap)
  (lambda (fn &rest args)
    (minibuffer-with-setup-hook
        (lambda ()
          (use-local-map
           (make-composed-keymap keymap (current-local-map))))
      (apply fn args))))

(defvar embark-completing-read-prompter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") 'abort-recursive-edit)
    map))

(advice-add 'embark-completing-read-prompter :around
            (with-minibuffer-keymap embark-completing-read-prompter-map))
(define-key vertico-map (kbd "C-<return>") 'embark-act-with-completing-read)

(defun embark-act-with-completing-read (&optional arg)
  (interactive "P")
  (let* ((embark-prompter 'embark-completing-read-prompter)
         (act (propertize "Act" 'face 'highlight))
         (embark-indicator (lambda (_keymap targets) nil)))
    (embark-act arg)))
;; elbow grease for embark -- end

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

;; Jumpt between test and impl files
(crafted-package-install-package 'toggle-test)

(define-key project-prefix-map (kbd "t") 'tgt-toggle)

;; stolen from https://gitlab.com/andreyorst/dotfiles/-/blob/master/.config/emacs/init.el
(defun pv-tgt-local-setup (&optional spec)
  (lambda ()
    (when-let* ((root (project-current))
                (project-root (project-root root)))
      (setq-local
       tgt-projects
       `(((:root-dir ,project-root)
          (:src-dirs ,(or (plist-get spec :src-dirs) "src"))
          (:test-dirs ,(or (plist-get spec :test-dirs) "test"))
          (:test-suffixes ,(plist-get spec :suffixes))
          (:test-prefixes ,(plist-get spec :prefixes))))))))

(dolist (hook '(clojure-mode-hook
                clojurec-mode-hook
                clojurescript-mode-hook))
  (add-hook hook (pv-tgt-local-setup '(:suffixes "_test"))))


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

(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

(crafted-package-install-package 'hydra)

(defhydra smartparens-hydra (:hint nil)
  "
  _w_: next        _>_: barf     _(_: wrap raound    _u_: splice    _d_: kill    _j_: down sexp    _q_: quit
  _W_: next lvl    _<_: slurp    _[_: wrap square    _r_: raise     _y_: copy    _k_: up sexp
  _b_: prev                      _{_: wrap curly                    _Y_: copy
  _B_: prev lvl
  "
  (">" sp-forward-barf-sexp)
  ("<" sp-forward-slurp-sexp)

  ("j" sp-down-sexp)
  ("k" sp-backward-up-sexp)
  ("w" sp-next-sexp)
  ("W" sp-beginning-of-next-sexp)
  ("b" sp-previous-sexp)
  ("B" sp-beginning-of-previous-sexp)
  ;; ("e" sp-end-of-next-sexp "End of Next sexp")

  ("(" sp-wrap-round :color blue)
  ("[" sp-wrap-square :color blue)
  ("{" sp-wrap-curly :color blue)

  ("u" sp-splice-sexp :color blue)
  ("r" sp-raise-sexp  :color blue)

  ("y" sp-copy-sexp  :color blue)
  ("Y" sp-backwards-copy-sexp  :color blue)
  ("d" sp-kill-sexp :color blue)
  ("q" nil :color blue))

(define-key smartparens-mode-map (kbd "M-s") 'smartparens-hydra/body)

(crafted-package-install-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))

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
                        (list
                         (cape-super-capf
                          (cape-capf-properties #'cider-complete-at-point :exclusive 'no)
                          #'cape-dabbrev)
                          #'cape-file))))

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
