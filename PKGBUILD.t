# 
# Lisp script deployment demo.
# Copyright (C) 2023 M E Leypold
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

pkgname=__PKGNAME__
pkgver=__PKGVER__
pkgrel=1
pkgdesc='A declarative interface to the Common Lisp command line parser clingon'
arch=('any')
license=('GPL')
depends=('common-lisp' 'cl-asdf' 'sbcl')
source=(
  "__PKGNAME__-__VERSION__.tar.gz"
)
b2sums=('SKIP')

package() {

    /bin/pwd
    make install DEST=STAGE PREFIX=/usr
    mv STAGE/* "$pkgdir"    
}

