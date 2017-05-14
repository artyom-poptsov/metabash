;; Copyright (C) 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (metabash)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (ssh session)
  #:use-module (ssh auth)
  #:use-module (ssh popen)
  #:use-module (ssh shell)
  #:use-module (common)
  #:export (host-accessible?
            application-available?
            local-command-available?
            establish-session!))


;;; Helper procedures.

(define (true=? string)
  (string=? string "true"))

(define (make-shell-predicate fmt . args)
  (string-append (apply format #f fmt args)
                 " 2>&1 > /dev/null && echo true || echo false"))


;;;

(define (host-accessible? host)
  "Check if a remote HOST is accessible with 'ping'."
  (let ((command (make-shell-predicate "ping -c1 ~a" host)))
    (format-debug "remote-host-accessible?: command: '~a'" command)
    (true=? (read-line (open-input-pipe command)))))

(define (establish-session! session)
  "Establish a SESSION with the remote host."
  (define (try action proc expected-result)
    (let ((result (proc session)))
      (unless (equal? result expected-result)
        (error (format #f "Could not ~a:" action) result session))))

  (try "connect to a server" connect! 'ok)
  (try "authenticate a server" authenticate-server 'ok)
  (try "authenticate with a public key" userauth-public-key/auto! 'success))

(define* (application-available? session url)
  (let ((command
         (cond
          ((command-available? session "curl")
           (make-shell-predicate "curl --silent --max-time 5 '~a'" url))
          ((command-available? session "wget")
           (make-shell-predicate "wget --quiet '~a'" url))
          (else
           (error "Remote host has neither 'curl' nor 'wget' installed"
                  session)))))
    (true=? (read-line (open-remote-input-pipe session command)))))

(define (local-command-available? command)
  (let ((p (open-input-pipe (make-shell-predicate "which '~a'" command))))
    (true=? (read-line p))))

;;; metabash.scm ends here.
