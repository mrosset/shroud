;;; Shroud
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; Shroud is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Shroud is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Shroud.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Development environment for GNU Guix.
;;
;;; Code:

(use-modules (guix packages)
             (guix licenses)
             (guix build-system gnu)
             (gnu))

(use-package-modules autotools pkg-config guile gnupg xdisorg)

(package
  (name "shroud")
  (version "0.1")
  (source #f)
  (build-system gnu-build-system)
  (native-inputs
   `(("pkg-config" ,pkg-config)
     ("autoconf" ,autoconf)
     ("automake" ,automake)))
  (inputs
   `(("guile" ,guile-2.0)
     ("gnupg" ,gnupg)
     ("xclip" ,xclip)))
  (synopsis "Simple password manager")
  (description "Shroud is a simple password manager with a
command-line interface.  The password database is stored as a Scheme
s-expression and encrypted with a GPG key.")
  (home-page "http://dthompson.us/pages/software/shroud.html")
  (license gpl3+))
