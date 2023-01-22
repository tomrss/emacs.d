EMACS := emacs
MANAGE_EL := bin/manage.el
CACHE_DIR := .cache
BACKUP_DIR := .backups
BACKUP_FILENAME := $(BACKUP_DIR)/emacs-cache-backup-$(shell date +'%s').tgz

EVAL = $(EMACS) --batch --load $(MANAGE_EL) --eval

install:
	$(EVAL) '(manage-install)'

upgrade:
	$(EVAL) '(manage-upgrade)'

build: rebuild

rebuild:
	$(EVAL) '(manage-rebuild)'

uninstall:
	rm -rvf straight/

backup:
	mkdir -p $(BACKUP_DIR)
	tar -czvf $(BACKUP_FILENAME) $(CACHE_DIR)

clean:
	rm -rvf $(CACHE_DIR)
