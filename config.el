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

;; Add terminal clipboard support
(crafted-package-install-package 'xclip)
(if (not (display-graphic-p))
    (xclip-mode 1))

(add-hook 'text-mode-hook (lambda () (setq mode-require-final-newline nil)))

;;; Litering
(crafted-package-install-package 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;; User Interface
(require 'crafted-ui)

(customize-set-variable 'display-fill-column-indicator-column 100)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Show column position in mode-line
(column-number-mode t)

;; highlight current line
(global-hl-line-mode t)

;; Install minions
(crafted-package-install-package 'minions)
(minions-mode t)

;; https://emacs.stackexchange.com/questions/2338/how-can-i-display-the-parent-directory-of-the-current-file-in-the-modeline
(setq-default mode-line-buffer-identification
              '(:eval (abbreviate-file-name default-directory)))

;; Set config variables
(custom-set-variables '(crafted-ui-display-line-numbers t)
                      '(crafted-ui-use-doom-modeline t)
                      '(crafted-startup-inhibit-splash t))


;;; Evil mode
(require 'crafted-evil)        ; An `evil-mode` configuration

;; Set configuration variables
(custom-set-variables '(crafted-evil-discourage-arrow-keys t)
                      '(evil-want-minibuffer nil)
                      '(evil-want-C-u-scroll nil))

(evil-set-initial-state 'Info-mode 'emacs)
(evil-set-initial-state 'package-menu-mode 'emacs)

;; Set preferred key bindings
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-M-j") 'consult-buffer)
(global-set-key (kbd "C-<return>") 'embark-act)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(define-key evil-motion-state-map (kbd "[ j") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "] j") 'evil-jump-forward)

(define-key evil-window-map (kbd "C-q") 'evil-quit)
(define-key evil-window-map (kbd "C-b") 'bookmark-jump-other-window)
(define-key evil-window-map (kbd "C-d") 'dired-other-window)
(define-key evil-window-map (kbd "C-M-j") 'consult-buffer-other-window)

;;; Completions and Actions
(require 'crafted-completion)  ; selection framework based on `vertico`

(custom-set-variables
 '(tab-always-indent 'complete)
 '(corfu-auto nil)
 '(corfu-separator ?\s)               ;; Orderless field separator
 '(corfu-quit-at-boundary 'separator) ;; Never quit at completion boundary
 '(corfu-quit-no-match nil)           ;; Never quit, even if there is no match
 '(corfu-preselect nil)         ;; Disable candidate preselection
 )

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
(require 'magit)
(require 'transient)

(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
(global-set-key (kbd "C-M-;") #'magit-status)

(crafted-package-install-package 'magit-delta)
(add-hook 'magit-mode-hook 'magit-delta-mode)

(crafted-package-install-package 'git-timemachine)


;;;; difftastic integration into magit
;;
;;   stolen from https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
;;;;
(defun th/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))

(defun th/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (th/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))

(defun th/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (th/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

(transient-define-prefix th/magit-aux-commands ()
  "My personal auxiliary magit commands."
  ["Auxiliary commands"
   ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
   ("s" "Difftastic Show" th/magit-show-with-difftastic)])

(transient-append-suffix 'magit-dispatch "!"
  '("#" "My Magit Cmds" th/magit-aux-commands))

(define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands)

;;; Project Management
(require 'crafted-project)

(define-key project-prefix-map (kbd "g") 'consult-ripgrep)

;; Make it possible to ignore risky local variables
(advice-add 'risky-local-variable-p :override #'ignore)

;; Returns the parent directory containing a .project.el file, if any,
;; to override the standard project.el detection logic when needed.
(defun pv-project-override (dir)
  (let ((override (seq-some
                   (lambda (file) (locate-dominating-file dir file))
                   '(".project.el" "deps.edn"))))
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

(custom-set-variables
 '(lsp-completion-provider :none))

;; https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless
(defun cw/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless))) ;; Configure orderless

(add-hook 'lsp-completion-mode #'cw/lsp-mode-setup-completion)

(setq lsp-eldoc-enable-hover t
      lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
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

(crafted-package-install-package 'lsp-java)
(add-hook 'java-mode-hook 'lsp)

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
  ("r" sp-raise-sexp)

  ("y" sp-copy-sexp  :color blue)
  ("Y" sp-backwards-copy-sexp  :color blue)
  ("d" sp-kill-sexp)
  ("q" nil :color blue))

(define-key smartparens-mode-map (kbd "M-s") 'smartparens-hydra/body)

(crafted-package-install-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; Clojure
(crafted-package-install-package 'cljstyle-format)

(custom-set-variables
 ;; cider-defun-at-point treats contents of comment block as top level functions
 '(clojure-toplevel-inside-comment-form t)
 ;; disable warning
 '(cljr-warn-on-eval nil))

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

(add-hook 'cider-mode-hook
          (lambda ()
            (define-key cider-mode-map (kbd "C-c M-l") #'cider-inspect-last-result)
            (setq-local completion-at-point-functions
                        (list
                         (cape-super-capf
                          (cape-capf-properties #'cider-complete-at-point :exclusive 'no)
                          (cape-capf-buster #'lsp-completion-at-point))
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
