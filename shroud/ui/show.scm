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

(define-module (shroud ui show)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (shroud utils)
  #:use-module (shroud secret)
  #:use-module (shroud ui)
  #:export (shroud-show))

(define (show-help)
  (format #t "Usage: shroud show [OPTION] id
Show secret named ID.~%")
  (display "
  -p, --password         show only the password")
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
                  (show-version-and-exit)))
        (option '(#\p "password") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'password #t result)))))

(define %default-options '())

(define (shroud-show config db . args)
  (let* ((opts      (args-fold args %options
                               (lambda (opt name arg result)
                                 (leave "~A: unrecognized option" name))
                               (lambda (arg result)
                                 (if (assq-ref result 'id)
                                     (leave "~A: extraneuous argument" arg)
                                     (alist-cons 'id arg result)))
                               %default-options))
         (id        (assq-ref opts 'id))
         (password? (assq-ref opts 'password)))

    (unless id
      (leave "no secret id specified"))

    (let* ((db     (secrets-by-id (force db)))
           (secret (vhash-ref db id)))
      (unless secret
        (leave "~a: secret undefined" id))

      (if password?
          (display (secret-ref secret "password"))
          (format #t "username: ~a~%password: ~a~%"
                  (secret-ref secret "username")
                  (secret-ref secret "password")))))

  ;; We don't alter the database.
  db)
