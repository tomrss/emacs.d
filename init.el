;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

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

;;;; Set default modules

(setq u/enabled-modules
      '(
        ;; key bindings
        "evil"

        ;; ui
        "icons"
        "doom-modeline"
        ;; "treemacs"

        ;; tools
        "org-roam"
        "vterm"

        ;;development
        "java"
        "go"
        "python"
        "node"
        "react"
        "groovy"
        ;; "kotlin"
        ;; "scala"
        ;; "clojure"
        "terraform"
        ;; "csharp"
        "http"
        ;; "kubernetes"
        ))

;;;; Load user local configuration

(load u/user-local-config t t)

;;;; Load modules

(u/module-load-modules)

;;; init.el ends here
