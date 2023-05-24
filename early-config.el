;;; early-config.el --- Personal early config        -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <vlko@archon>
;; Keywords: elisp

;; Fullscreen by default, as early as possible. This tiny window is not enough
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; Font Faces
(add-hook 'emacs-startup-hook
          (lambda ()
            (custom-set-faces
             `(default ((t (:font "JetBrains Mono Light 11"))))
             `(fixed-pitch ((t (:inherit (default)))))
             `(fixed-pitch-serif ((t (:inherit (default)))))
             `(variable-pitch ((t (:font "FreeSans 11")))))))

;;; Theme
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-region '(bg-only))

(progn
  (disable-theme 'deeper-blue)
  (load-theme 'modus-vivendi t))
