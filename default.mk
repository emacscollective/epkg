TOP := $(dir $(lastword $(MAKEFILE_LIST)))

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

DOMAIN      ?= emacsmirror.net
CFRONT_DIST ?= E1IXJGPIOM4EUW

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../../,$(DEPS))
LOAD_PATH  += -L .

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = -L ../../org/lisp
ORG_LOAD_PATH += -L ../../ox-texinfo+
endif

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

STATS_DIR ?= $(TOP)docs/stats
