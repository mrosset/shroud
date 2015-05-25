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
  #:use-module (srfi srfi-9)
  #:export (make-secret
            secret?
            secret-id
            secret-content
            alist->secret
            secret->alist
            load-secrets
            save-secrets))

(define-record-type <secret>
  (make-secret id content)
  secret?
  (id      secret-id)
  (content secret-content))

(define (alist->secret alist)
  "Convert ALIST into a <secret> record."
  (make-secret (assq-ref alist 'id) (assq-ref alist 'content)))

(define (secret->alist secret)
  "Convert SECRET into an alist."
  (match secret
    (($ <secret> id content)
     `((id . ,id)
       (content . ,content)))))

(define (load-secrets file)
  "Load secrets from FILE, or return '() if FILE does not exist."
  (if (file-exists? file)
      (map alist->secret (call-with-input-file file read))
      '()))

(define (save-secrets secrets file)
  "Write SECRETS to FILE."
  (call-with-output-file file
    (lambda (port)
      (write (map secret->alist secrets) port))))
