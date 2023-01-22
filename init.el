;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; GNU Emacs configuration.

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (locate-user-emacs-file "modules/")))

(require 'mod-init-directory)
(require 'mod-defaults)
(require 'mod-packaging)
(require 'mod-keys)
(require 'mod-ui)
(require 'mod-completions)
(require 'mod-editing)
(require 'mod-window)
(require 'mod-terminals)
(require 'mod-org)
(require 'mod-development)

;;; init.el ends here
