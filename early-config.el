;;; early-config.el --- Personal early config        -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <vlko@archon>
;; Keywords: elisp

;; Set further font and theme customizations
(custom-set-variables
   '(rational-ui-default-font
     '(:font "JetBrains Mono" :weight regular :height 110)))


;; Fullscreen by default, as early as possible. This tiny window is not enough
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
