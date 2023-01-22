;;; mod-init-directory.el --- Cleanup init dir module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Module for cleaning up init directory from every "cache" file.  A
;; cache file is a file containing persistent data for any Emacs
;; feature or package.  By default, they are often saved flat in init
;; directory (`user-emacs-directory'), but this causes that directory
;; to become cluttered with files that are not actually part of the
;; real user Emacs code.

;;; Code:

(defvar +emacs-cache-directory (locate-user-emacs-file ".cache/")
  "Location where to save Emacs cache files.")

(defun +locate-emacs-cache-file (filename)
  "Locate FILENAME in Emacs cache directory."
  (expand-file-name (convert-standard-filename filename)
                    +emacs-cache-directory))

(defalias '+cache-file '+locate-emacs-cache-file)

(make-directory +emacs-cache-directory t)
(make-directory (+cache-file "auto-save/") t)

(setq savehist-file                     (+cache-file "history"))
(setq save-place-file                   (+cache-file "places"))
(setq project-list-file                 (+cache-file "projects"))
(setq recentf-save-file                 (+cache-file "recentf"))
(setq straight-base-dir                 +emacs-cache-directory)
(setq package-user-dir                  (+cache-file "elpa"))
(setq eshell-directory-name             (+cache-file "eshell"))
(setq undo-tree-history-directory-alist `(("." . ,(+cache-file "undo"))))
(setq transient-levels-file             (+cache-file "transient/levels.el"))
(setq transient-values-file             (+cache-file "transient/values.el"))
(setq transient-history-file            (+cache-file "transient/history.el"))
(setq backup-directory-alist            `(("." . ,(+cache-file "backups"))))
(setq auto-save-file-name-transforms    `((".*" ,(+cache-file "auto-save/") t)))
(setq auto-save-list-file-prefix        (+cache-file "auto-save/sessions/"))
(setq tramp-auto-save-directory         (+cache-file "tramp/auto-save/"))
(setq tramp-persistency-file-name       (+cache-file "tramp/persistency.el"))
(setq scratch-directory                 (+cache-file "scratch/"))
(setq bookmark-file                     (+cache-file "bookmarks"))
(setq org-id-locations-file             (+cache-file ".org-id-locations"))
(setq org-roam-db-location              (+cache-file "org-roam.db"))
(setq url-cache-directory               (+cache-file "url/cache/"))

(provide 'mod-init-directory)
;;; mod-init-directory.el ends here
