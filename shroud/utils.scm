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

(define-module (shroud utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:export (vhash-ref
            vhash-replace
            vhash-values
            gpg-binary
            call-with-encrypted-output-file
            call-with-decrypted-input-file))

(define (vhash-ref vhash key)
  "Return the value associated with KEY in VHASH or #f if there is no
such key."
  (match (vhash-assoc key vhash)
    ((_ . value) value)
    (_ #f)))

(define (vhash-replace key value vhash)
  "Replace the association of KEY with VALUE in VHASH."
  (vhash-cons key value (vhash-delete key vhash)))

(define (vhash-values vhash)
  "Return a list of the values within VHASH."
  (vhash-fold-right (lambda (key value result)
                      (cons value result))
                    '() vhash))

(define gpg-binary (make-parameter "gpg"))

(define (call-with-pipe* program+args mode proc)
  "Apply PROC with an open pipe in the given MODE for the subprocess
COMMAND+ARGS."
  (let ((pipe (apply open-pipe* mode program+args)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc pipe))
      (lambda ()
        (close-pipe pipe)))))

(define (call-with-output-pipe* program+args proc)
  "Apply PROC with an open output pipe for the subprocess
PROGRAM+ARGS."
  (call-with-pipe* program+args OPEN_WRITE proc))

(define (call-with-input-pipe* program+args proc)
  "Apply PROC with an open input pipe for the subprocess
PROGRAM+ARGS."
  (call-with-pipe* program+args OPEN_READ proc))

(define (call-with-encrypted-output-file file user-id proc)
  "Apply PROC with an output port that writes encrypted data to FILE
for the recipient USER-ID."
  (call-with-output-pipe* `(,(gpg-binary)
                            "--no-tty" "--batch" "--yes"
                            "--encrypt" "--armor"
                            "--recipient" ,user-id
                            "--output" ,file)
    proc))

(define (call-with-decrypted-input-file file proc)
  "Apply PROC with an input port containing the decrypted contents of
FILE."
  ;; Suppress info/debug/error messages.
  (call-with-output-file "/dev/null"
    (lambda (port)
      (parameterize ((current-error-port port))
        (call-with-input-pipe* `(,(gpg-binary)
                                 "--no-tty" "--batch" "--yes"
                                 "--decrypt" ,file)
          proc)))))
