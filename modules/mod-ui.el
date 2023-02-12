;;; mod-ui.el --- User interface module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Module for enhancing and personalizing user interface, including
;; fonts, icons, modeline, theme, dired and other.

;;; Code:

(eval-and-compile
  (require 'project))

;; recognize system
(defconst IS-GNU     (eq system-type 'gnu/linux))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;;;; Fonts and icons

(when (display-graphic-p)
  (when (x-list-fonts "JetBrains Mono NL")
    (set-face-attribute 'default     nil :font "JetBrains Mono NL" :height 110 :weight 'normal)
    (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono NL" :height 110 :weight 'normal))
  (when (x-list-fonts "Cantarell")
    (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 130 :weight 'normal)))

(+use-package 'all-the-icons)
(when (display-graphic-p)
  (require 'all-the-icons nil nil)
  (unless (x-list-fonts "all-the-icons")
    (if IS-WINDOWS
	    (warn "RuntimeWarning M-x all-the-icons-install-fonts to download the fonts, then install them manually")
      (all-the-icons-install-fonts t))))

;;;; Theme

;; use doom themes
;; (+use-package 'doom-themes)
;; (load-theme 'doom-nord t)
;; (set-face-attribute 'font-lock-doc-face nil :foreground "#EBCB8B")
;; (set-face-attribute 'completions-annotations nil :foreground "#EBCB8B")

(setq modus-themes-mode-line '(accented))
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(load-theme 'modus-vivendi)

;;;; Modeline

(+use-package 'doom-modeline)
(setq doom-modeline-icon (display-graphic-p))
(doom-modeline-mode +1)

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

(+use-package 'visual-fill-column)

(defun +setup-visual-fill (width)
  "Setup visual line and column centered with WIDTH."
  (setq visual-fill-column-width width)
  (setq visual-fill-column-center-text t)
  (visual-line-mode +1)
  (visual-fill-column-mode +1))

;;;; Starting screen

;; i wrote this package and it's not great
;; TODO at least add a readme in it
;; TODO make it private because it sucks
(if (eq +packaging-system 'straight)
    (+use-package '(welcome :type git
                            :host github
                            :repo "tomrss/welcome.el"
                            :files ("welcome.el" "asset")))
  ;; TODO this is very wrong. if is ugly, and it requires to having used straight
  (add-to-list 'load-path "~/.emacs.d/.cache/straight/repos/welcome.el/")
  (require 'welcome))

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
           :action +edit-emacs-config
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

  (evil-set-initial-state 'welcome-mode 'emacs)
  (define-key welcome-mode-map (kbd "j") #'next-line)
  (define-key welcome-mode-map (kbd "k") #'previous-line)
  (add-hook 'welcome-mode-hook
            (lambda () (+setup-visual-fill welcome-window-width))))

(add-hook 'emacs-startup-hook #'welcome-screen)

(provide 'mod-ui)
;;; mod-ui.el ends here
