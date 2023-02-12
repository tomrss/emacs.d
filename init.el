;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; GNU Emacs configuration.

;;; Code:

;;;; add modules and custom lisp directories to load path

(eval-and-compile
  (defvar u/user-modules-directory
    (locate-user-emacs-file "modules/")
    "Directory of user init modules.")

  (defvar u/user-lisp-directory
    (locate-user-emacs-file "lisp/")
    "Directory of user custom Lisp.")

  (add-to-list 'load-path u/user-modules-directory)
  (add-to-list 'load-path u/user-lisp-directory))

;;;; process autoloads of custom lisp

(let ((custom-lisp-autoloads
       (expand-file-name "autoloads" u/user-lisp-directory)))
  (loaddefs-generate u/user-lisp-directory custom-lisp-autoloads)
  (load custom-lisp-autoloads nil t))

;;;; require modules

(require 'mod-init-directory)
(require 'mod-defaults)
(require 'mod-packaging)
(require 'mod-keys)
(require 'mod-ui)
(require 'mod-completions)
(require 'mod-editing)
(require 'mod-window)
(require 'mod-management)
(require 'mod-org)
(require 'mod-terminals)
(require 'mod-development)

;;; init.el ends here
