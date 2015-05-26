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

(define-module (shroud ui)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-37)
  #:use-module (shroud config)
  #:use-module (shroud utils)
  #:use-module (shroud secret)
  #:export (simple-args-fold
            program-name
            show-version-and-exit
            leave
            make-user-module
            load*
            shroud-main))

(define (simple-args-fold args options default-options)
  (args-fold args options
             (lambda (opt name arg result)
               (leave "~A: unrecognized option" name))
             (lambda (arg result)
               (leave "~A: extraneuous argument" arg))
             default-options))

(define program-name (make-parameter "shroud"))

(define %commands
  '("hide" "show" "remove"))

(define (show-help)
  (format #t "Usage: shroud COMMAND ARGS...
Run COMMAND with ARGS.~%~%")
  (format #t "COMMAND may be one of the sub-commands listed below:~%~%")
  (format #t "~{   ~a~%~}" %commands))

(define (show-usage)
  (format #t "Try `shroud --help' for more information.~%")
  (exit 1))

(define (show-version-and-exit)
  (format #t "~a ~a
Copyright (C) 2015 David Thompson <davet@gnu.org>
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.~%"
          (program-name) %shroud-version)
  (exit 0))

(define (leave format-string . args)
  "Display error message and exist."
  (apply format (current-error-port) format-string args)
  (newline)
  (exit 1))

(define (make-user-module modules)
  "Return a new user module with the additional MODULES loaded."
  ;; Module in which the machine description file is loaded.
  (let ((module (make-fresh-user-module)))
    (for-each (lambda (iface)
                (module-use! module (resolve-interface iface)))
              modules)
    module))

(define (report-load-error file args)
  "Report the failure to load FILE, a user-provided Scheme file, and exit.
ARGS is the list of arguments received by the 'throw' handler."
  (match args
    (('system-error . _)
     (let ((err (system-error-errno args)))
       (leave "failed to load '~a': ~a~%" file (strerror err))))
    (('syntax-error proc message properties form . rest)
     (let ((file (assq-ref properties 'filename))
           (line (assq-ref properties 'line))
           (col  (assq-ref properties 'column)))
       (format (current-error-port) "~a:~a:~a: error: ~a~%"
               file (and line (1+ line)) col message))
     (exit 1))
    ((error args ...)
     (format (current-error-port) "failed to load '~a':~%" file)
     (apply display-error #f (current-error-port) args)
     (exit 1))))

(define (load* file user-module)
  "Load the user provided Scheme source code FILE."
  (catch #t
    (lambda ()
      (set! %fresh-auto-compile #t)

      (save-module-excursion
       (lambda ()
         (set-current-module user-module)
         (primitive-load file))))
    (lambda args
      (report-load-error file args))))

(define %default-config
  `((database-file . ,(string-append (getenv "HOME") "/.shroud-db"))
    (gpg-binary    . "gpg")))

(define (load-config)
  "Load and evaluate user configuration file."
  (let ((config (append (load* (string-append (getenv "HOME") "/.shroud")
                               (make-user-module '((shroud config))))
                        %default-config)))

    (unless (assq-ref config 'user-id)
      (leave "user-id must be specified in configuration"))

    config))

(define (command-proc command)
  "Return the procedure for COMMAND."
  (let* ((module
             (catch 'misc-error
               (lambda ()
                 (resolve-interface `(shroud ui ,command)))
               (lambda -
                 (format (current-error-port) "~a: invalid subcommand~%" command)
                 (show-usage)))))
    (module-ref module (symbol-append 'shroud- command))))

(define (option? str)
  "Return #t if STR is an option string."
  (string-prefix? "-" str))

(define (make-program-name command)
  "Return a program name string for COMMAND."
  (string-append "shroud " (symbol->string command)))

(define (shroud-main . args)
  (match args
    (() (show-usage))
    ((or ("-h") ("--help"))
     (show-help))
    (("--version")
     (show-version-and-exit))
    (((? option? opt) _ ...)
     (format (current-error-port) "shroud: unrecognized option '~a'~%" opt))
    (((= string->symbol command) . args)
     (let* ((config  (load-config))
            (db-file (assq-ref config 'database-file))
            (user-id (assq-ref config 'user-id))
            (gpg     (assq-ref config 'gpg-binary)))
       (parameterize ((gpg-binary gpg))
         ;; Don't load database until needed to avoid pinentry prompt
         ;; when running commands like 'shroud show --help'.
         (let* ((db     (delay
                          (catch 'decrypt-fail
                            (lambda () (load-secrets db-file))
                            (lambda (key file)
                              (leave "~a: could not decrypt database" file)))))
                (proc   (command-proc command))
                (result (parameterize ((program-name (make-program-name
                                                      command)))
                          (apply proc config db args))))
           (unless (eq? db result)
             (save-secrets result db-file user-id))))))))
