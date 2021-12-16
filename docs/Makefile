-include ../config.mk
include ../default.mk

docs: info html html-dir pdf

info: $(PKG).info dir
html: $(PKG).html
pdf:  $(PKG).pdf

ORG_ARGS  = --batch -Q $(ORG_LOAD_PATH) -l ox-texinfo+
ORG_EVAL  = --eval "(setq org-texinfo+-dissolve-noexport-headlines t)"
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

.PHONY: stats
stats:
	@printf "Generating statistics\n"
	@gitstats -c style=https://magit.vc/assets/stats.css -c max_authors=999 $(TOP) $(STATS_DIR)

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

CLEAN = $(PKG).info dir $(PKG) $(PKG).html $(PKG).pdf

clean:
	@printf "Cleaning docs/*...\n"
	@rm -rf $(CLEAN)