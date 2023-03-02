;;; eplm.el --- Emacs Package Lifecycle Manager -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Emacs Package Lifecycle Manager for avoiding long running
;; blocking operations at Emacs startup.  It is intended to
;; be run in batch mode and not from a running Emacs instance.

;; EPLM provides batch functions to manage the lifecyle of Emacs
;; packages, such as installation, upgrades and builds.

;; It relies on the `straight' package manager and require an
;; Emacs init file that uses `straight'.

;;; Code:

(defconst eplm-packaging-system
  (intern (or (getenv "EMACS_PACKAGING_SYSTEM") "straight"))
  "Packaging system to use.  Currently only `straight' is fully supported.")

(defun eplm--load-init-files ()
  "Load init files."
  (load-file "early-init.el")
  (load-file "init.el"))

(defun eplm-install ()
  "Install packages defined in init files."
  (message "Installing packages...")
  (eplm--load-init-files)
  (when (eq eplm-packaging-system 'straight)
    (let ((lockfile (cdr (assq nil straight-profiles))))
      (unless (file-exists-p lockfile)
        (straight-freeze-versions))))
  (message "Installation complete"))

(defun eplm-upgrade ()
  "Upgrade packages defined in init files."
  (message "Upgrading packages...")
  (eplm--load-init-files)
  (unless (eq eplm-packaging-system 'straight)
    (user-error "Unable to upgrade for packaging %s" eplm-packaging-system))
  (straight-pull-all)
  (straight-freeze-versions)
  (message "Upgrade complete."))

(defun eplm-rebuild ()
  "Rebuild packages."
  (eplm--load-init-files))

(defun eplm-sync-from-lockfile ()
  "Sync packages versions from the ones in the lockfile."
  (unless (eq eplm-packaging-system 'straight)
    (user-error "Unable to sync from lockfile for packaging %s" eplm-packaging-system))
  (eplm--load-init-files)
  (straight-thaw-versions))

(provide 'eplm)
;;; eplm.el ends here
