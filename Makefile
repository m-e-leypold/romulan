# Romulan - Declarative interface to the clingon command line argument parser.
# Copyright (C) 2023  M E Leypold
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# 

# * Targets --------------------------------------------------

all::
install::
live-install::
.PHONY: @always

.ONESHELL:

# * Software name --------------------------------------------

SHORT-NAME ?= $(shell echo "$(notdir $(CURDIR))" | sed 's|_.*||')

# * Installation ---------------------------------------------
# ** Parameters ----------------------------------------------

DEST       ?= $(CURDIR)/.stage
PREFIX     ?= /usr/local
LISPDIR    ?= $(PREFIX)/share/common-lisp/source/$(SHORT-NAME)
BINDIR     ?= $(PREFIX)/bin

LISPFILES  ?= $(wildcard *.lisp *.asd)
BINFILES   ?= $(patsubst ./%,%,\
                 $(shell find . -maxdepth 1 -mindepth 1 -executable -type f))

LISPFILES   := $(strip $(LISPFILES))
BINFILES    := $(strip $(BINFILES))

USER-BINDIR   ?= $(lastword $(wildcard ~/my/scripts/bin ~/my/bin ~/bin ~/.local/bin))

USER-LISPROOT ?= $(lastword \
                     $(wildcard ~/share/common-lisp/source ~/my/asdf ~/.asdf))

USER-LISPDIR  := $(USER-LISPROOT)/$(SHORT-NAME)


$(info PREFIX       = $(PREFIX))
$(info DEST         = $(DEST))
$(info BINDIR       = $(BINDIR))
$(info LISPDIR      = $(LISPDIR))
$(info USER-BINDIR  = $(USER-BINDIR))
$(info USER-LISPDIR = $(USER-LISPDIR))

ifneq ($(LISPFILES),)
  install:: install-lisp
  live-install:: live-install-lisp
endif

ifneq ($(BINFILES),)
  install:: install-bin
  live-install:: live-install-bin
endif

# ** Staging for packaging (or direct installation) ----------

$(BINFILES:%=.build/bin/%): .build/bin/%: %
	set -eu
	mkdir -p "$(@D)"
	sed < "$<" '/^[#][!]/s|^[#][!].*|#!/usr/bin/sbcl --script|' >$@

install-lisp:
	set -eu
	mkdir -p $(DEST)$(LISPDIR)
	install -m 644 $(LISPFILES) $(DEST)$(LISPDIR)

install-bin: $(BINFILES:%=.build/bin/%)
	set -eu
	mkdir -p $(DEST)$(BINDIR)
	install -m 755 $(BINFILES:%=.build/bin/%) $(DEST)$(BINDIR)/

clean::
	rm -rf $(DEST)

# ** Live installation (with links) --------------------------

live-install-lisp:
	set -eu
	rm -f $(USER-LISPDIR)
	ln -s $(CURDIR) $(USER-LISPDIR)

live-install-bin:
	set -eu
	ln -sf $(BINFILES:%=$(CURDIR)/%) $(USER-BINDIR)/

# * Arch packages --------------------------------------------

VERSION := $(shell git describe --tags)
PKGVER  := $(shell echo "$(VERSION)" | sed 's|[-]|+|g' | tr '[A-Z]' '[a-z]')
TARFILE := $(SHORT-NAME)-$(VERSION).tar.gz
PACKAGE := $(SHORT-NAME)-$(PKGVER)-1-any.pkg.tar.zst

$(info VERSION  = $(VERSION))
$(info PKGVER   = $(PKGVER))
$(info PACKAGE  = $(PACKAGE))
$(info TARFILE  = $(TARFILE))

.build/arch-package/PKGBUILD.$(VERSION):: PKGBUILD.t
	set -eu
	mkdir -p "$(@D)"
	sed <$< \
            's|__PKGVER__|$(PKGVER)|g;s|__PKGNAME__|$(SHORT-NAME)|g;s|__VERSION__|$(VERSION)|' \
            >$@

.build/arch-package/PKGBUILD: .build/arch-package/PKGBUILD.$(VERSION)
	set -eu
	mkdir -p "$(@D)"
	cp $< $@

.build/arch-package/$(TARFILE):: .build/arch-package/PKGBUILD.$(VERSION)
	set -eu
	mkdir -p "$(@D)"
	git archive -o "$@" HEAD

.build/arch-package/$(PACKAGE): .build/arch-package/$(TARFILE) .build/arch-package/PKGBUILD
	set -eu
	cd .build/arch-package/
	rm -rf pkg src
	makepkg -f

package: .build/arch-package/$(PACKAGE)

clean::
	rm -rf .stage .build

# * Project integration --------------------------------------

-include Project/Project.mk

Project:
	git clone -b project --single-branch . Project

project-setup: Project
	make git-setup

# * Epilog ---------------------------------------------------

$(info )

