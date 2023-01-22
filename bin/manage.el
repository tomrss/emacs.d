;;; update.el --- Emacs package updater -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Update Emacs packages with the `straight' package manager.

;;; Code:

(defun manage--load-init-files ()
  "Load init files."
  (load-file "early-init.el")
  (load-file "init.el"))

(defun manage-install ()
  "Install packages defined in init files."
  (message "Installing packages...")
  (manage--load-init-files)
  (when (eq +packaging-system 'straight)
    (let ((lockfile (cdr (assq nil straight-profiles))))
      (unless (file-exists-p lockfile)
        (straight-freeze-versions))))
  (message "Installation complete"))

(defun manage-upgrade ()
  "Upgrade packages defined in init files."
  (message "Upgrading packages...")
  (manage--load-init-files)
  (unless (eq +packaging-system 'straight)
    (user-error "Unable to upgrade for packaging %s" +packaging-system))
  (straight-pull-all)
  (straight-freeze-versions)
  (message "Upgrade complete."))

(defun manage-rebuild ()
  "Rebuild packages."
  (manage--load-init-files))

;;; manage.el ends here
