;;; core-ui.el --- User interface customization -*- lexical-binding: t -*-

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

;; Module for enhancing and personalizing user interface, including
;; fonts, icons, modeline, theme and other.

;;; Code:

(require 'core-keys)
(require 'core-packaging)
(require 'core-utils)

;;;; Fonts and icons

(defvar u/try-fonts-default
  '("JetBrains Mono NL"
    "JetBrains Mono")
  "Try to load one of this fonts (first that works wins) as default font.")

(defvar u/try-fonts-variable-pitch
  '("Cantarell")
  "Try to load one of this fonts (first that works wins) as variable pitch font.")

(when (display-graphic-p)
  (when-let ((default-font (u/find-first 'x-list-fonts u/try-fonts-default)))
    (set-face-attribute 'default     nil :font default-font :height 110 :weight 'normal)
    (set-face-attribute 'fixed-pitch nil :font default-font :height 110 :weight 'normal))
  (when-let ((variable-font (u/find-first 'x-list-fonts u/try-fonts-variable-pitch)))
    (set-face-attribute 'variable-pitch nil :font variable-font :height 130 :weight 'normal)))

;;;; Theme

(defconst u/theme
  (intern (or (getenv "EMACS_THEME") "modus-vivendi"))
  "Theme to use.")

(cond
 ((eq u/theme 'modus-vivendi)
  ;; configure modus-vivendi theme
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-org-blocks 'gray-background)
  (load-theme 'modus-vivendi)
  (set-face-attribute 'modus-themes-heading-1 nil :foreground "#f78fe7"))
 ((eq u/theme 'doom-nord)
  ;; configure nord theme
  (u/use-package 'doom-themes)
  (setq doom-nord-brighter-modeline t)
  (load-theme 'doom-nord t)
  (set-face-attribute 'query-replace nil :background "#BF616A")
  (set-face-attribute 'font-lock-doc-face nil :foreground "#EBCB8B")
  (set-face-attribute 'completions-annotations nil :foreground "#EBCB8B"))
 (t
  (load-theme u/theme t)))

;;;; Modeline

;; TODO we want to set a fallback for the absence of doom modeline,
;; but this isn't right because core should not depend on modules.
;; define the concept of "module fallback" somehow
;; or accept that minions package will be loaded also in case of doom.
(require 'core-modules)
(unless (member "doom-modeline" u/enabled-modules)
  (u/use-package 'minions)
  (minions-mode +1))

;;;; Line and column numbers

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(column-number-mode +1)

;;;; Smooth scrolling

(unless (version< emacs-version "29")
  (pixel-scroll-precision-mode +1))
(setq fast-but-imprecise-scrolling t)
(setq scroll-margin 1)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 1)

;;;; Highlight current line

(setq hl-line-sticky-flag nil)
(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'special-mode-hook #'hl-line-mode)

;;;; Visual fill mode

(u/use-package 'visual-fill-column)

(defun u/setup-visual-fill (width)
  "Setup visual line and column centered with WIDTH."
  (when (fboundp 'visual-fill-column-mode)
    (setq visual-fill-column-width width)
    (setq visual-fill-column-center-text t)
    (visual-line-mode +1)
    (visual-fill-column-mode +1)))

;;;; Scratch buffers

;; TODO use package-vc-install in the u/use-package macro
(when (eq u/packaging-system 'straight)
  (u/use-package '(scratch-el :type git
			                  :host github
			                  :repo "tomrss/scratch.el")))

(with-eval-after-load 'scratch
  (setq scratch-search-fn #'consult-ripgrep)
  (scratch-persist-mode +1))

(u/define-key (kbd "C-c s") 'scratch-key-map)

;;;; Starting screen

;; TODO use package-vc-install in the u/use-package macro
(when (eq u/packaging-system 'straight)
  (u/use-package '(welcome :type git
                           :host github
                           :repo "tomrss/welcome.el"
                           :files ("welcome.el" "asset"))))

(with-eval-after-load 'welcome
  (setq welcome-menu-items
        '(("Recent files"
           :key "f"
           :action consult-recent-file
           :icon (all-the-icons-octicon . "history"))
          ("Projects"
           :key "p"
           :action project-switch-project
           :icon (all-the-icons-octicon . "repo"))
          ("Dired"
           :key "d"
           :action dired
           :icon (all-the-icons-octicon . "file-directory"))
          ("Edit configuration"
           :key "c"
           :action u/edit-emacs-config
           :icon (all-the-icons-octicon . "gear"))
          ("Kubernetes"
           :key "u"
           :action kubernetes-overview
           :icon (all-the-icons-octicon . "server"))
          ("Eshell"
           :key "e"
           :action eshell
           :icon (all-the-icons-octicon . "terminal"))
          ("Vterm"
           :key "v"
           :action vterm
           :icon (all-the-icons-faicon . "terminal"))
          ("Scratch"
           :key "s"
           :action scratch-new
           :icon (all-the-icons-octicon . "file-text"))
          ("Bookmarks"
           :key "b"
           :action bookmark-jump
           :icon (all-the-icons-octicon . "bookmark"))
          ("Org Roam note"
           :key "r"
           :action org-roam-node-find
           :icon (all-the-icons-octicon . "checklist"))
          ("Org Roam daily"
           :key "y"
           :action org-roam-dailies-capture-today
           :icon (all-the-icons-octicon . "calendar"))
          ("EWW browser"
           :key "w"
           :action eww
           :icon (all-the-icons-octicon . "globe"))))

  (define-key welcome-mode-map (kbd "j") #'next-line)
  (define-key welcome-mode-map (kbd "k") #'previous-line)
  (add-hook 'welcome-mode-hook
            (lambda () (u/setup-visual-fill welcome-window-width))))

(add-hook 'emacs-startup-hook #'welcome-screen)

(provide 'core-ui)
;;; core-ui.el ends here
