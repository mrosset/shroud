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
  (format #t "Usage: shroud show [OPTION] ID [KEY ...]
Show secret named ID.~%")
  (display "
  -c, --clipboard        copy output to the X clipboard")
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
        (option '("clipboard" #\c) #f #f
                (lambda (opt name arg result)
                  (alist-cons 'clipboard? #t result)))))

(define %default-options '())

(define (process-args args)
  (args-fold args %options
             (lambda (opt name arg result)
               (leave "~A: unrecognized option" name))
             (lambda (arg result)
               (if (assq-ref result 'id)
                   (alist-cons 'key arg result)
                   (alist-cons 'id arg result)))
             %default-options))

(define* (display-secret secret keys #:optional (port (current-output-port)))
  (match keys
    (()
     (for-each (match-lambda
                ((key . value)
                 (format port "~a\t~a~%" key value)))
               (secret-contents secret)))
    ((keys ...)
     (for-each (match-lambda
                ((key . value)
                 (when (member key keys)
                   (format port "~a~%" value))))
               (secret-contents secret)))))

(define (shroud-show config db . args)
  (let* ((opts       (process-args args))
         (id         (leave-if-false (assq-ref opts 'id)
                                     "no secret ID given"))
         (keys       (alist-pick opts 'key))
         (clipboard? (assq-ref opts 'clipboard?))
         (secret     (vhash-ref (secrets-by-id (force db)) id)))

    (unless secret
      (leave "secret '~a' does not exist" id))

    (if clipboard?
        (call-with-clipboard
         (lambda (port)
           (display-secret secret keys port)
           (display "copied secret to clipboard\n"
                    (current-error-port))))
        (display-secret secret keys))

    ;; Database remains unaltered.
    db))
