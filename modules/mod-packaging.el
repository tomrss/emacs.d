;;; mod-packaging.el --- Packaging module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Module for configuring packaging.

;;; Code:

;;; Configure `straight.el' as package manager

(defconst +packaging-system
  (intern (or (getenv "EMACS_PACKAGING_SYSTEM") "straight"))
  "Packaging system to use.
Choice between `straight', `builtin', `none'")

;; initialize packaging
(cond
 ((eq +packaging-system 'straight)
  ;; bootstrap `straight.el'
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (+locate-emacs-cache-file "straight/repos/straight.el/bootstrap.el"))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))
 ((eq +packaging-system 'builtin)
  ;; initialize builtin packaging system
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))
 ((eq +packaging-system 'none)
  ;; do nothing
  nil))


;; configure straight lockfile (it can be committed)
(setq straight-profiles
      `((nil . ,(expand-file-name "lockfile.el" user-emacs-directory))))

;; use package custom macro
(defmacro +use-package (package)
  "Use PACKAGE."
  (cond
   ((eq +packaging-system 'straight)
    `(straight-use-package ,package))
   ((eq +packaging-system 'builtin)
    `(unless (package-installed-p ,package)
       (package-install ,package)))
   ((eq +packaging-system 'none)
    nil)))

(provide 'mod-packaging)
;;; mod-packaging.el ends here
