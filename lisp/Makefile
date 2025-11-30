-include ../config.mk
include ../default.mk

lisp: $(ELCS) autoloads check-declare

autoloads: $(PKG)-autoloads.el

$(PKG).elc:
$(PKG)-desc.elc: $(PKG).elc
$(PKG)-elpa.elc: $(PKG).elc
$(PKG)-list.elc: $(PKG).elc $(PKG)-desc.elc
$(PKG)-org.elc: $(PKG).elc
$(PKG)-schemata.elc: $(PKG).elc
$(PKG)-utils.elc: $(PKG).elc

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --funcall batch-byte-compile $<

check-declare:
	@printf " Checking function declarations\n"
	@$(EMACS_BATCH) --eval "(check-declare-directory default-directory)"

CLEAN = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf " Cleaning lisp/*...\n"
	@rm -rf $(CLEAN)

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS_BATCH) --load autoload --eval "\
(let* ((file (expand-file-name \"$@\"))\
       (generated-autoload-file file)\
       (coding-system-for-write 'utf-8-emacs-unix)\
       (backup-inhibited t)\
       (version-control 'never)\
       (inhibit-message t))\
  (write-region (autoload-rubric file \"package\" t) nil file)\
  (update-directory-autoloads default-directory))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"
