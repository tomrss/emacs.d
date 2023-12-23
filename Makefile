### @Manage user Emacs packages lifecyle@

# Copyright (C) 2023  Tommaso Rossi

## Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

## This file is NOT part of GNU Emacs.

### Commentary:

# Manage lifecycle of user Emacs packages by providing some bindings
# for the Emacs Package Lifecycle Manager.

# make install
#		Install all Emacs packages in the user init file.
#		Use it when this repo is cloned, before starting
#		Emacs for the first time.
#
# make upgrade
#		Upgrade all packages to latest versions in the package archives.
#		It will modify the lockfile accordingly.
#		Use it when full package upgrade is needed.
# 		Test it a little before committing the lockfile.
#		Optionally add "rebuild" command to avoid latency in the
#		successive Emacs startup.
#
# make sync-from-lockfile
#		Sync current installation with versions specified in lockfile.
#		Do it after pulling commits with modifications to lockfile.
#		Optionally add "rebuild" command to avoid latency in the
#		successive Emacs startup.
#
# make build
# make rebuild
#		(they are the same)
#		Build all packages after modifications in the packages versions.
#		Optionally use it after upgrade or sync for avoiding latency
#		in the successive (to version modifications) Emacs startup.
#
# make backup
#		Backup the current state (recentf, history, ...)
#
# make uninstall
#		Uninstall all packages
#
# make clean
#		Clean the user Emacs folder from every package and state file.


EMACS := emacs
EPLM := bin/eplm.el
ROOT_DIR := $(shell pwd)
CACHE_DIR := .cache
PACKAGES_DIR := .cache/straight
BACKUP_DIR := .backups
BACKUP_FILENAME := $(BACKUP_DIR)/emacs-cache-backup-$(shell date +'%s').tgz

ELISP_FUNCALL = $(EMACS) --init-directory=$(ROOT_DIR)  --batch --load $(EPLM) --funcall

install:
	$(ELISP_FUNCALL) eplm-install

upgrade:
	$(ELISP_FUNCALL) eplm-upgrade

sync-from-lockfile:
	$(ELISP_FUNCALL) eplm-sync-from-lockfile

build: rebuild

rebuild:
	$(ELISP_FUNCALL) eplm-rebuild

uninstall:
	rm -rvf $(PACKAGES_DIR)

backup:
	mkdir -p $(BACKUP_DIR)
	tar -czvf $(BACKUP_FILENAME) $(CACHE_DIR)

clean:
	rm -rvf $(CACHE_DIR)
