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

(define-module (shroud config)
  #:export (%shroud-version
            %shroud-gpg-binary
            %shroud-database-file
            %shroud-user-id))

(define %shroud-version "0.1")
(define %shroud-gpg-binary "gpg")
(define %shroud-database-file
  (string-append (getenv "HOME") "/.shroud-db"))
(define %shroud-user-id #f)
