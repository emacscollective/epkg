-include config.mk

PKG = epkg

ELS   = $(PKG).el
ELS  += $(PKG)-desc.el
ELS  += $(PKG)-list.el
ELS  += $(PKG)-gelpa.el
ELS  += $(PKG)-melpa.el
ELS  += $(PKG)-org.el
ELS  += $(PKG)-schemata.el
ELS  += $(PKG)-utils.el
ELCS  = $(ELS:.el=.elc)

DEPS  = closql
DEPS += dash
DEPS += emacsql

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = -L ../org/lisp
ORG_LOAD_PATH += -L ../org-contrib/lisp
ORG_LOAD_PATH += -L ../ox-texinfo+
endif

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

all:  lisp info
docs: info html html-dir pdf

help:
	$(info make all          - generate lisp and manual)
	$(info make docs         - generate most manual formats)
	$(info make lisp         - generate byte-code and autoloads)
	$(info make texi         - generate texi manual (see comments))
	$(info make info         - generate info manual)
	$(info make html         - generate html manual file)
	$(info make html-dir     - generate html manual directory)
	$(info make pdf          - generate pdf manual)
	$(info make preview      - preview html and pdf manuals)
	$(info make publish      - publish html and pdf manuals)
	$(info make clean        - remove most generated files)
	@printf "\n"

lisp: $(ELCS) loaddefs

loaddefs: $(PKG)-autoloads.el

$(PKG).elc:
$(PKG)-desc.elc: $(PKG).elc
$(PKG)-list.elc: $(PKG).elc

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -f batch-byte-compile $<

info: $(PKG).info dir
html: $(PKG).html
pdf:  $(PKG).pdf

ORG_ARGS  = --batch -Q $(ORG_LOAD_PATH) -l ox-extra -l ox-texinfo+.el
ORG_EVAL  = --eval "(ox-extras-activate '(ignore-headlines))"
ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --funcall org-texinfo-export-to-texinfo

# This target first bumps version strings in the Org source.  The
# necessary tools might be missing so other targets do not depend
# on this target and it has to be run explicitly when appropriate.
#
#   AMEND=t make texi    Update manual to be amended to HEAD.
#   VERSION=N make texi  Update manual for release.
#
.PHONY: texi
texi:
	@$(EMACS) $(ORG_ARGS) $(PKG).org $(ORG_EVAL)
	@printf "\n" >> $(PKG).texi
	@rm -f $(PKG).texi~

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: $(PKG).info
	@printf "Generating $@\n"
	@printf "%s" $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

%.html: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --html --no-split $(MANUAL_HTML_ARGS) $<

html-dir: $(PKG).texi
	@printf "Generating $(PKG)/*.html\n"
	@$(MAKEINFO) --html $(MANUAL_HTML_ARGS) $<

%.pdf: %.texi
	@printf "Generating $@\n"
	@texi2pdf --clean $< > /dev/null

DOMAIN         ?= emacsmirror.net
CFRONT_DIST    ?= E1IXJGPIOM4EUW
PUBLISH_PATH   ?= /manual/
PUBLISH_BUCKET ?= s3://$(DOMAIN)
PREVIEW_BUCKET ?= s3://preview.$(DOMAIN)
PUBLISH_TARGET ?= $(PUBLISH_BUCKET)$(PUBLISH_PATH)
PREVIEW_TARGET ?= $(PREVIEW_BUCKET)$(PUBLISH_PATH)

preview: html html-dir pdf
	@aws s3 cp $(PKG).html $(PREVIEW_TARGET)
	@aws s3 cp $(PKG).pdf $(PREVIEW_TARGET)
	@aws s3 sync $(PKG) $(PREVIEW_TARGET)$(PKG)/

publish: html html-dir pdf
	@aws s3 cp $(PKG).html $(PUBLISH_TARGET)
	@aws s3 cp $(PKG).pdf $(PUBLISH_TARGET)
	@aws s3 sync $(PKG) $(PUBLISH_TARGET)$(PKG)/
	@printf "Generating CDN invalidation\n"
	@aws cloudfront create-invalidation \
	--distribution-id $(CFRONT_DIST) --paths "\
$(PUBLISH_PATH)$(PKG).html,\
$(PUBLISH_PATH)$(PKG).pdf,\
$(PUBLISH_PATH)$(PKG)/*" > /dev/null

CLEAN  = $(ELCS) $(PKG)-autoloads.el $(PKG).info dir
CLEAN += $(PKG) $(PKG).html $(PKG).pdf

clean:
	@printf "Cleaning...\n"
	@rm -rf $(CLEAN)

define LOADDEFS_TMPL
;;; $(PKG)-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name \
(or (file-name-directory #$$) (car load-path))))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; $(PKG)-autoloads.el ends here
endef
export LOADDEFS_TMPL
#'

$(PKG)-autoloads.el: $(ELS)
	@printf "Generating $@\n"
	@printf "%s" "$$LOADDEFS_TMPL" > $@
	@$(EMACS) -Q --batch --eval "(progn\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"
