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
  (format #t "Usage: shroud hide [OPTION] ID KEY=VALUE ...
Add a new secret named ID to the database.~%")
  (display "
  -e, --edit             replace existing username/password if it exists")
  (display "
  -h, --help             display this help and exit")
  (display "
  --version              display version information and exit")
  (newline))

(define %options
  (list (option '(#\e "edit") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'edit? #t result)))
        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '("version") #f #f
                (lambda args
                  (show-version-and-exit)))))

(define %default-options '())

(define (process-args args)
  (args-fold args %options
             (lambda (opt name arg result)
               (leave "~A: unrecognized option" name))
             (lambda (arg result)
               ;; The first unnamed argument is the secret id.  Each
               ;; subsequent argument is a key/value pair.
               (if (assq-ref result 'id)
                   (match (string-split arg #\=)
                     ((key value)
                      (alist-cons 'secret (cons key value) result))
                     (_ (leave "~A: invalid key/value pair" arg)))
                   (alist-cons 'id arg result)))
             %default-options))

(define (shroud-hide config db . args)
  (let* ((opts     (process-args args))
         (id       (assq-ref opts 'id))
         (contents (filter-map (match-lambda
                                 (('secret . pair) pair)
                                 (_ #f))
                               opts))
         (edit?    (assq-ref opts 'edit?)))

    (unless id
      (leave "no secret id specified"))
    (when (null? contents)
      (leave "no key/value pairs specified"))

    (let* ((db       (secrets-by-id (force db)))
           (existing (vhash-ref db id))
           (vcons    (if existing vhash-replace vhash-cons)))

      (when (and (not edit?) existing)
        (leave "~a: secret already defined" id))
      (when (and edit? (not existing))
        (leave "~a: secret undefined" id))

      (let* ((contents (if edit?
                           (alist-compact
                            (append contents (secret-contents existing)))
                           contents))
             (secret   (make-secret id contents)))
        (vhash-values (vcons id secret db))))))
