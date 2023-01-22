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

;;;; Directory tree view

(+use-package 'treemacs)
(+use-package 'treemacs-all-the-icons)
(+define-key (kbd "C-x c t") #'treemacs-select-window)
(with-eval-after-load 'treemacs
  (treemacs-project-follow-mode t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-commit-diff-mode t)
  (treemacs-hide-gitignored-files-mode nil)

  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons"))

(+use-package 'treemacs-evil)
(with-eval-after-load 'evil
  (with-eval-after-load 'treemacs
    (require 'treemacs-evil)))

(+use-package 'treemacs-magit)
(with-eval-after-load 'magit
  (with-eval-after-load 'treemacs
    (require 'treemacs-magit)))

;;;; Line and column numbers

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(column-number-mode +1)

;;;; Smooth scrolling

(pixel-scroll-precision-mode +1)
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

;;;; File management

;; configure Dired
(with-eval-after-load 'dired
  ;; dired defaults
  (setq delete-by-moving-to-trash t)
  (setq dired-auto-revert-buffer t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always) ; no problem, it goes to thrash
  (setq dired-create-destination-dirs 'ask)
  (setq dired-listing-switches "-agho --group-directories-first")
  (setq dired-use-ls-dired (not IS-MAC))

  ;; enable dired-x for omit mode and enhanced find file commands
  (setq dired-x-hands-off-my-keys nil)
  (setq dired-omit-files "\\`[.]?#\\|\\`[.].*\\'")
  (require 'dired-x)
  (evil-define-key 'normal dired-mode-map (kbd "g h") #'dired-omit-mode))

(+define-key (kbd "C-x j") #'dired-jump)

;; use icons in dired
(+use-package 'all-the-icons-dired)
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

;; colorize dired
(+use-package 'diredfl)
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'diredfl-mode))

;;;; Process management

(with-eval-after-load 'proced
  (setq proced-auto-update-interval 5)
  (proced-toggle-auto-update 1))

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
