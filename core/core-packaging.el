;;; core-packaging.el --- Setup package loading -*- lexical-binding: t -*-

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

;; Module for configuring packaging and packaging loading.

;;; Code:

(require 'core-init-directory)

;; TODO with module system, environment variable for chosing packaging seems not appropriate

(defconst u/packaging-system
  (intern (or (getenv "EMACS_PACKAGING_SYSTEM") "straight"))
  "Packaging system to use.
Choice between `straight', `builtin', `none'")

;;;; Initialize packaging

(cond
 ((eq u/packaging-system 'straight)
  ;; bootstrap `straight.el'
  (setq straight-repository-branch "develop")
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (u/locate-cache-file "straight/repos/straight.el/bootstrap.el"))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  ;; configure straight lockfile (it can be committed)
  (setq straight-profiles
        `((nil . ,(expand-file-name "lockfile.el" user-emacs-directory))))
  ;; exclude built-in packages from straight
  (dolist (package '(org
                     eglot
                     jsonrpc
                     package
                     eldoc
                     xref
                     flymake))
    (straight-use-package `(,package :type built-in))))
 ((eq u/packaging-system 'builtin)
  ;; initialize builtin packaging system
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))
 ((eq u/packaging-system 'none)
  ;; do nothing
  nil))

;;;; Use package custom macro

(defmacro u/use-package (package)
  "Use PACKAGE."
  (cond
   ((eq u/packaging-system 'straight)
    `(straight-use-package ,package))
   ((eq u/packaging-system 'builtin)
    `(unless (package-installed-p ,package)
       (package-install ,package)))
   ((eq u/packaging-system 'none)
    nil)))

(provide 'core-packaging)
;;; core-packaging.el ends here
