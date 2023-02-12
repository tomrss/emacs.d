;;; mod-init-directory.el --- Cleanup init dir module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Module for cleaning up init directory from every "cache" file.
;; A cache file is a file containing persistent data for any Emacs
;; feature or package.  By default, they are often saved flat in init
;; directory (`user-emacs-directory'), but this causes that directory
;; to become cluttered with files that are not actually part of the
;; real user Emacs code.

;;; Code:

(defvar u/emacs-cache-directory (locate-user-emacs-file ".cache/")
  "Location where to save Emacs cache files.")

(defun u/locate-emacs-cache-file (filename)
  "Locate FILENAME in Emacs cache directory."
  (expand-file-name (convert-standard-filename filename)
                    u/emacs-cache-directory))

(defalias 'u/cache-file 'u/locate-emacs-cache-file)

(make-directory u/emacs-cache-directory t)
(make-directory (u/cache-file "auto-save/") t)

(setq savehist-file                     (u/cache-file "history"))
(setq save-place-file                   (u/cache-file "places"))
(setq project-list-file                 (u/cache-file "projects"))
(setq recentf-save-file                 (u/cache-file "recentf"))
(setq straight-base-dir                 u/emacs-cache-directory)
(setq package-user-dir                  (u/cache-file "elpa"))
(setq eshell-directory-name             (u/cache-file "eshell"))
(setq undo-tree-history-directory-alist `(("." . ,(u/cache-file "undo"))))
(setq transient-levels-file             (u/cache-file "transient/levels.el"))
(setq transient-values-file             (u/cache-file "transient/values.el"))
(setq transient-history-file            (u/cache-file "transient/history.el"))
(setq backup-directory-alist            `(("." . ,(u/cache-file "backups"))))
(setq auto-save-file-name-transforms    `((".*" ,(u/cache-file "auto-save/") t)))
(setq auto-save-list-file-prefix        (u/cache-file "auto-save/sessions/"))
(setq tramp-auto-save-directory         (u/cache-file "tramp/auto-save/"))
(setq tramp-persistency-file-name       (u/cache-file "tramp/persistency.el"))
(setq scratch-directory                 (u/cache-file "scratch/"))
(setq bookmark-file                     (u/cache-file "bookmarks"))
(setq org-id-locations-file             (u/cache-file ".org-id-locations"))
(setq org-roam-db-location              (u/cache-file "org-roam.db"))
(setq url-cache-directory               (u/cache-file "url/cache/"))

(provide 'mod-init-directory)
;;; mod-init-directory.el ends here
