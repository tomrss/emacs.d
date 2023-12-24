;;; early-init.el --- Emacs early init -*- lexical-binding: t -*-

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

;; Emacs early init file, that gets evaluated before init.el and even before the GUI is loaded.
;; The focus is on optimizing startup time and perform basic UI cleanups.

;;; Code:

;;;; Move native compilation cache outside init dir

(when (not (getenv "CI"))
  (when (and (fboundp 'native-compile)
             (fboundp 'startup-redirect-eln-cache))
    (let ((eln-cache-dir "~/.cache/emacs/eln-cache/"))
      (make-directory eln-cache-dir t)
      (startup-redirect-eln-cache
       (convert-standard-filename eln-cache-dir)))))

;;;; Optimize garbage collections

(defconst +gc-cons-standard-threshold-mb 1024
  "Number of MB of consing between garbage collection during normal operativity.")

(defconst +gc-cons-startup-threshold-mb 2048
  "Number of MB of consing between garbage collection during startup.")

(defun +restore-garbage-collection ()
  "Restore GC consing threshold to `+gc-cons-standard-threshold-mb'."
  (setq gc-cons-threshold (* +gc-cons-standard-threshold-mb 1024 1024)))

;; set high garbage collection consing threshold during startup
(setq gc-cons-threshold (* +gc-cons-startup-threshold-mb 1024 1024))

;; restore garbage collection after init
(add-hook 'emacs-startup-hook '+restore-garbage-collection)

;; garbage collect when idle
(run-with-idle-timer 2 t 'garbage-collect)

;;;; Temporary disable file handler at startup

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun +reset-file-name-handler-alist ()
  "Reset filename handlers to default value."
  (setq file-name-handler-alist default-file-name-handler-alist))

(add-hook 'emacs-startup-hook '+reset-file-name-handler-alist)

;;;; Disable package.el at startup

(setq package-enable-at-startup nil)

;;;; Early UI cleanups

;; frame resize seems to be very expensive, disable it
(setq frame-inhibit-implied-resize t)
(setq inhibit-default-init t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; initial frame settings
(setq default-frame-alist
      `(
        ;; start fullscreen without window bar
        ;; (undecorated . t)
        ;; start fullscreen
        (fullscreen . maximized)
        ;; avoid blinding white on startup
        (background-color . "#000000")))

(set-face-attribute 'default nil :foreground "#ffffff")

(when (not (getenv "CI"))
  ;; disable unwanted ui components
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (setq inhibit-startup-screen t))

;;; early-init.el ends here
