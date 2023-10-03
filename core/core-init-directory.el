;;; core-init-directory.el --- Cleanup init dir -*- lexical-binding: t -*-

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

;; Module for cleaning up init directory from every "cache" file.
;; A cache file is a file containing persistent data for any Emacs
;; feature or package.  By default, they are often saved flat in init
;; directory (`user-emacs-directory'), but this causes that directory
;; to become cluttered with files that are not actually part of the
;; real user Emacs code.

;;; Code:

(defconst u/cache-directory (locate-user-emacs-file ".cache/")
  "Location where to save cache files of Emacs and packages.")

(defun u/locate-cache-file (filename)
  "Locate FILENAME in Emacs cache directory."
  (expand-file-name (convert-standard-filename filename)
                    u/cache-directory))

(defalias 'u/cache-file 'u/locate-cache-file)

(make-directory u/cache-directory t)
(make-directory (u/cache-file "auto-save/") t)

(setq savehist-file                     (u/cache-file "history"))
(setq save-place-file                   (u/cache-file "places"))
(setq project-list-file                 (u/cache-file "projects"))
(setq recentf-save-file                 (u/cache-file "recentf"))
(setq straight-base-dir                 u/cache-directory)
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

(provide 'core-init-directory)
;;; core-init-directory.el ends here
