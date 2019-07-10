## Shroud
## Copyright © 2015 David Thompson <davet@gnu.org>
## Copyright (C) 2019 by Amar Singh<nly@disroot.org>
##
## Shroud is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Shroud is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Shroud.  If not, see <http://www.gnu.org/licenses/>.

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

CLEANFILES = $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

bin_SCRIPTS =					\
  scripts/shroud

SOURCES = 					\
  shroud/config.scm				\
  shroud/utils.scm				\
  shroud/secret.scm				\
  shroud/ui.scm					\
  shroud/ui/hide.scm				\
  shroud/ui/list.scm				\
  shroud/ui/remove.scm				\
  shroud/ui/show.scm

EXTRA_DIST +=					\
  pre-inst-env.in				\
  package.scm

publish: distcheck
	gpg --sign --detach-sign --armor --yes shroud-$(VERSION).tar.gz && \
        scp shroud-$(VERSION).tar.gz shroud-$(VERSION).tar.gz.asc \
	    blog@dthompson.us:/var/www/files/shroud/
