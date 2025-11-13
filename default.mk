TOP := $(dir $(lastword $(MAKEFILE_LIST)))

PKG = epkg

ELS   = $(PKG).el
ELS  += $(PKG)-desc.el
ELS  += $(PKG)-elpa.el
ELS  += $(PKG)-list.el
ELS  += $(PKG)-org.el
ELS  += $(PKG)-schemata.el
ELS  += $(PKG)-utils.el
ELCS  = $(ELS:.el=.elc)

DEPS  = closql
DEPS += compat
DEPS += emacsql
DEPS += llama
# Optional:
DEPS += cond-let
DEPS += magit/lisp

DOMAIN      ?= emacsmirror.org
CFRONT_DIST ?= E1IXJGPIOM4EUW

VERSION ?= $(shell test -e $(TOP).git && git describe --tags --abbrev=0 | cut -c2-)
REVDESC := $(shell test -e $(TOP).git && git describe --tags)

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../../,$(DEPS))
LOAD_PATH  += -L .

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = -L ../../org/lisp
endif

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

GITSTATS      ?= gitstats
GITSTATS_DIR  ?= $(TOP)docs/stats
GITSTATS_ARGS ?= -c style=https://magit.vc/assets/stats.css -c max_authors=999
