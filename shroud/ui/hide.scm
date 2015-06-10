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

(define-module (shroud ui hide)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (shroud utils)
  #:use-module (shroud secret)
  #:use-module (shroud ui)
  #:export (shroud-hide))

(define (show-help)
  (format #t "Usage: shroud hide [OPTION] --id=ID --username=USERNAME --password=PASSWORD
Add a new secret to the database.~%")
  (display "
  -i, --id               unique ID for secret")
  (display "
  -u, --username         username")
  (display "
  -p, --password         password")
  (display "
  -r, --replace          replace existing username/password if it exists")
  (display "
  -h, --help             display this help and exit")
  (display "
  --version              display version information and exit")
  (newline))

(define %options
  (list (option '(#\i "id") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'id arg result)))
        (option '(#\u "username") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'username arg result)))
        (option '(#\p "password") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'password arg result)))
        (option '(#\r "replace") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'replace? #t result)))
        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '("version") #f #f
                (lambda args
                  (show-version-and-exit)))))

(define %default-options '())

(define (shroud-hide config db . args)
  (let* ((opts     (simple-args-fold args %options %default-options))
         (id       (assq-ref opts 'id))
         (username (assq-ref opts 'username))
         (password (assq-ref opts 'password))
         (replace? (assq-ref opts 'replace?)))

    (unless id
      (leave "no secret id specified"))
    (unless username
      (leave "no username specified"))
    (unless password
      (leave "no password specified"))

    (let* ((db       (secrets-by-id (force db)))
           (existing (vhash-ref db id))
           (vcons    (if existing vhash-replace vhash-cons))
           (contents `(("username" . ,username)
                       ("password" . ,password)))
           (secret   (make-secret id contents)))

      (when (and (not replace?) existing)
        (leave "~a: secret already defined" id))

      (vhash-values (vcons id secret db)))))
