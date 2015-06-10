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

(define-module (shroud secret)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (shroud utils)
  #:export (make-secret
            secret?
            secret-id
            secret-contents
            secret-ref
            alist->secret
            secret->alist
            load-secrets
            save-secrets
            secrets-by-id))

(define-record-type <secret>
  (make-secret id contents)
  secret?
  (id       secret-id)
  (contents secret-contents))

(define (secret-ref secret key)
  "Return the secret data associated with KEY in SECRET."
  (assoc-ref (secret-contents secret) key))

(define (alist->secret alist)
  "Convert ALIST into a <secret> record."
  (make-secret (assq-ref alist 'id)
               (assq-ref alist 'contents)))

(define (secret->alist secret)
  "Convert SECRET into an alist."
  (match secret
    (($ <secret> id contents)
     `((id . ,id)
       (contents . ,contents)))))

(define (load-secrets file)
  "Load secrets from FILE, or return '() if FILE does not exist."
  (if (file-exists? file)
      (map alist->secret
           ;; Handle existing file that isn't PGP encrypted.
           (let ((stored (call-with-decrypted-input-file file read)))
             (if (eof-object? stored)
                 (throw 'decrypt-fail file)
                 stored)))
      '()))

(define (save-secrets secrets file user-id)
  "Write SECRETS to FILE, encrypted for USER-ID."
  (call-with-encrypted-output-file file user-id
    (lambda (port)
      (write (map secret->alist secrets) port))))

(define (secrets-by-id secrets)
  "Convert the list SECRETS into a vhash keyed off of the secret id."
  (fold (lambda (secret result)
          (vhash-cons (secret-id secret) secret result))
        vlist-null secrets))
