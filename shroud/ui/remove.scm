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

(define-module (shroud ui remove)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (shroud utils)
  #:use-module (shroud secret)
  #:use-module (shroud ui)
  #:export (shroud-remove))

(define (show-help)
  (format #t "Usage: shroud remove [OPTION] id
Remove a secret from the database.~%")
  (display "
  -h, --help             display this help and exit")
  (display "
  --version              display version information and exit")
  (newline))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '("version") #f #f
                (lambda args
                  (show-version-and-exit)))))

(define %default-options '())

(define (shroud-remove config db . args)
  (let* ((opts (args-fold args %options
                          (lambda (opt name arg result)
                            (leave "~A: unrecognized option" name))
                          (lambda (arg result)
                            (if (assq-ref result 'id)
                                (leave "~A: extraneuous argument" arg)
                                (alist-cons 'id arg result)))
                          %default-options))
         (id   (assq-ref opts 'id))
         (db   (secrets-by-id (force db))))

    (unless (vhash-ref db id)
      (leave "secret '~a' is undefined" id))

    (vhash-values (vhash-delete id db))))
