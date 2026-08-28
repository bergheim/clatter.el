;;; clatter-evil.el --- Evil bindings for clatter's special buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Normal-state bindings for clatter's read-only special buffers, where
;; evil's state maps shadow the major-mode keymaps (RET becomes
;; `evil-ret', q records a macro).  Inert without evil: the bindings
;; install via `with-eval-after-load', and only `evil-define-key*' (a
;; function, unlike the `evil-define-key' macro) is used so this file
;; byte-compiles in evil-free environments.

;;; Code:

(require 'clatter-nicklist)
(require 'clatter-feed)

(declare-function evil-define-key* "evil-core")

(defun clatter-evil--setup ()
  "Install normal-state bindings for clatter's special buffers."
  (evil-define-key* 'normal clatter-feed-mode-map
    (kbd "RET") #'clatter-feed-visit
    "q" #'quit-window)
  (evil-define-key* 'normal clatter-nicklist-mode-map
    (kbd "RET") #'clatter-nicklist-query
    "q" #'clatter-nicklist-close
    "gr" #'clatter-nicklist-refresh))

(with-eval-after-load 'evil
  (clatter-evil--setup))

(provide 'clatter-evil)

;;; clatter-evil.el ends here
