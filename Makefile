-include config.mk

ELS   = epkg.el
ELS  += epkg-desc.el
ELS  += epkg-list.el
ELCS  = $(ELS:.el=.elc)

DEPS  = closql
DEPS += dash
DEPS += emacsql
DEPS += finalize

LOADDEFS = epkg-autoloads.el

EMACS  ?= emacs
EFLAGS ?=
DFLAGS ?= $(addprefix -L ../,$(DEPS))
MFLAGS ?= -L ../org/lisp -L ../ox-texinfo+

INFOPAGES     = epkg.info
MAKEINFO     ?= makeinfo --no-split
INSTALL_INFO ?= $(shell command -v ginstall-info || printf install-info)

.PHONY: help texi clean

all: lisp info

help:
	$(info make all       - generate lisp and manual)
	$(info make lisp      - generate byte-code and loaddefs)
	$(info make info      - generate manual)
	$(info make texi      - generate (tracked) texi file)
	$(info make clean     - remove generated files)
	@printf "\n"

lisp: $(ELCS) loaddefs

epkg.elc:
epkg-desc.elc: epkg.elc
epkg-list.elc: epkg.elc

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EFLAGS) -L . $(DFLAGS) -f batch-byte-compile $<

loaddefs: $(LOADDEFS)

define LOADDEFS_TMPL
;;; $(LOADDEFS) --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name \
(or (file-name-directory #$$) (car load-path))))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; $(LOADDEFS) ends here
endef
export LOADDEFS_TMPL
#'

$(LOADDEFS): $(ELS)
	@printf "Generating $@\n"
	@printf "%s" "$$LOADDEFS_TMPL" > $@
	@$(EMACS) -Q --batch --eval "(progn\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory)))"

info: $(INFOPAGES) dir

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) $< -o $@

texi:
	@printf "Generating epkg.texi\n"
	@$(EMACS) -Q --batch $(EFLAGS) $(DFLAGS)
	-l ox-texinfo+.el epkg.org \
	-f org-texinfo+export-to-texinfo
	@echo >> epkg.texi
	@rm -f epkg.texi~

dir: $(INFOPAGES)
	@printf "Generating $@\n"
	@$(INSTALL_INFO) $< --dir=$@

clean:
	@printf "Cleaning...\n"
	@rm -f $(ELCS) $(LOADDEFS) $(INFOPAGES)
