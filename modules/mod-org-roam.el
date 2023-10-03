;;; mod-org-roam.el --- Setup Org Roam for note taking  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi@protonmail.com>

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

;; Setup Org Roam for note taking with defaults and some common
;; capture templates, including gpg encrypted one.

;;; Code:

(u/use-package 'org-roam)
(defvar u/org-roam-base-dir "~/.org-roam")
(add-to-list 'recentf-exclude u/org-roam-base-dir)
(setq org-roam-v2-ack t)
(u/define-key (kbd "C-c n l") #'org-roam-buffer-toggle)
(u/define-key (kbd "C-c n f") #'org-roam-node-find)
(u/define-key (kbd "C-c n i") #'org-roam-node-insert)
(autoload 'org-roam-dailies-map "org-roam-dailies" nil nil 'keymap)
(u/define-key (kbd "C-c n d") 'org-roam-dailies-map)

(with-eval-after-load 'org-roam
  (make-directory u/org-roam-base-dir t)
  (setq org-roam-directory u/org-roam-base-dir)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("s" "secure" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>.org.gpg"
                              "#+title: ${title}\n")
           :unnarrowed t)))
  (define-key org-mode-map (kbd "C-i") #'completion-at-point)
  (org-roam-setup))

(provide 'mod-org-roam)
;;; mod-org-roam.el ends here
