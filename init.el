;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; GNU Emacs configuration.

;;; Code:

;;;; Add core and modules to load path

(eval-and-compile
  (defconst u/core-directory
    (locate-user-emacs-file "core/")
    "Directory of core modules.")

  (defconst u/modules-directory
    (locate-user-emacs-file "modules/")
    "Directory of user customizable modules.")

  (defconst u/user-local-config
    (locate-user-emacs-file "local-config.el")
    "Local config of the user.")


  (add-to-list 'load-path u/core-directory)
  (add-to-list 'load-path u/modules-directory))

;;;; Load core features

(require 'core-init-directory)
(require 'core-defaults)
(require 'core-ui)
(require 'core-completions)
(require 'core-window)
(require 'core-dired)
(require 'core-orgmode)
(require 'core-terminals)
(require 'core-development)
(require 'core-modules)
(cond
 ((eq system-type 'darwin)
  (require 'core-mac)))

;;;; Set default modules

(defvar u/default-modules
  '(
    "evil"                ; vim emulation with evil mode
    "icons"               ; use icons in dired, completions and other
    "doom-modeline"       ; fancy modeline from doom emacs
    "modus-vivendi-theme" ; high-contrast dark theme
    ;; "nord-theme"       ; low-contrast dark theme based on Nord Theme
    ;; "treemacs"         ; directory tree view
    "tempel"              ; template of code snippets
    "org-roam"            ; org roam note taking tool
    "vterm"               ; vterm is a fast terminal emulator written in C
    "tree-sitter"         ; use tree-sitter for syntax highlighting (require build option)
    "go"                  ; development with go
    "python"              ; development with python
    "scheme"              ; scheme and guile hacking
    "java"                ; development with java
    "groovy"              ; development with groovy
    ;; "kotlin"           ; development with kotlin
    ;; "scala"            ; development with scala
    ;; "clojure"          ; development with clojure
    "node"                ; development with node
    "react"               ; development with react
    ;; "csharp"           ; development with csharp
    "erlang"              ; development with erlang
    ;; "php"              ; development with php
    "terraform"           ; iac with terraform
    "http"                ; http client
    ;; "kubernetes"       ; kubernetes overview buffer
    "copilot"             ; github copilot
    )
  "Default modules loaded.  Override them in `u/user-local-config' file.")

(setq u/enabled-modules u/default-modules)

;;;; Load user local configuration

(load u/user-local-config t t)

;;;; Load modules

(u/module-load-modules)

;;; init.el ends here
