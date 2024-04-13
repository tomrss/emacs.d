;;; core-modules.el --- Module loading and management  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Tommaso Rossi

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

;; Primitives for loading and managing modules.  A "module" in this
;; context is an Emacs Lisp file containing a set of pluggable
;; non-core features.  Module files start with "mod-" and are
;; contained in the `u/modules-directory'.

;;; Code:

(defvar u/enabled-modules nil
  "List of module names to load.")

(defvar u/enable-all-modules (getenv "EMACS_ENABLE_ALL_MODULES")
  "If non-nil, load all modules.")

(define-error 'no-such-module "No such module")

(defun u/module--load-module (name)
  "Require module with NAME."
  (condition-case nil
      (require (intern name))
    (file-missing
     (signal 'no-such-module `(,name)))))

(defun u/module-load-modules ()
  "Load pluggable module.
Modules are specified in `u/modules'.  If `u/enable-all-modules', load
all modules instead."
  (if u/enable-all-modules
      (dolist (module-file (directory-files u/modules-directory t "mod-.*\\.el"))
        (u/module--load-module (file-name-base module-file)))
    (dolist (module-name u/enabled-modules)
      (u/module--load-module (format "mod-%s" module-name)))))

(provide 'core-modules)
;;; core-modules.el ends here
