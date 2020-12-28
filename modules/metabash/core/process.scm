;;; process.scm -- Metabash process implementation.

;; Copyright (C) 2020 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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


;;; Commentary:

;; This file contains implementation of Metabash processes.


;;; Code:

(define-module (metabash core process)
  #:use-module (oop goops)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ssh popen)
  #:use-module (ssh session)
  #:use-module (ssh dist)
  #:export (<process>
            process?
            process-input-port
            process-output-port
            process-state
            process-host
            process-command
            process-fifo-name
            process-start!
            process-started?
            process-stop!
            process-stopped?
            make-remote-fifo))

(define-class <process> ()
  ;; <symbol>
  ;;
  ;; Process state.  Available states:
  ;;     stopped, started
  (state       #:accessor     process-state
               #:setter       process-state-set!
               #:init-value   'stopped)

  ;; <session> | #f
  (host        #:accessor     process-host
               #:init-value   #f
               #:init-keyword #:host)
  ;; <string>
  (command     #:accessor     process-command
               #:init-value   "/bin/true"
               #:init-keyword #:command)
  ;; <string>
  (fifo-name   #:accessor     process-fifo-name
               #:init-value   #f)
  ;; <port>
  (input-port  #:accessor     process-input-port
               #:init-value   #f)
  ;; <port>
  (output-port #:accessor     process-output-port
               #:init-value   #f))

(define (process? x)
  (is-a? x <process>))



(define-method (process-stopped? (proc <process>))
  (equal? (process-state proc) 'stopped))

(define-method (process-started? (proc <process>))
  (equal? (process-state proc) 'started))



(define-method (display (proc <process>) (port <port>))
  (format port "#<process ~a~a~a ~a~a ~a>"
          (if (or (process-stopped? proc)
                  (port-closed? (process-input-port proc)))
              "x"
              "=")
          (if (process-started? proc)
              "="
              "x")
          (if (or (process-stopped? proc)
                  (port-closed? (process-output-port proc)))
              "x"
              "=")

          (let ((host (process-host proc)))
            (cond
             ((string? host)
              (string-append host ": "))
             ((session? host)
              (string-append (session-get host 'host) ": "))
             (else
              "")))

          (car (string-split (process-command proc) #\space))

          (number->string (object-address pipe) 16)))

(define-method (write (proc <process>) (port <port>))
  (display proc port))

(define-method (display (proc <process>))
  (next-method)
  (display proc (current-output-port)))

(define-method (write (proc <process>))
  (next-method)
  (display proc (current-output-port)))



(define (make-remote-fifo session)
  "Make a remote FIFO using a SSH session.  Return the FIFO name."
  (read-line
   (open-remote-input-pipe
    session
    "export NAME=$(mktemp --dry-run) &&  mkfifo ${NAME} && echo ${NAME}")))



(define-method (process-start! (proc <process>))
  (when (process-started? proc)
    (error "The process is already started" proc))
  (let ((host        (process-host proc))
        (fifo-name   (process-fifo-name proc))
        (command     (process-command proc)))
    (cond
     ((not host)
      (let ((fifo-name (tmpnam)))
        (system (string-append "mkfifo " fifo-name))
        (let ((input-port  (open-output-pipe
                            (string-append command " > " fifo-name)))
              (output-port (open-input-pipe
                            (string-append "cat " fifo-name))))
          (slot-set! proc 'input-port  input-port)
          (slot-set! proc 'output-port output-port)
          (slot-set! proc 'fifo-name   fifo-name))))
     ((session? host)
      (let* ((fifo-name   (make-remote-fifo host))
             (input-port  (open-remote-output-pipe
                           host
                           (string-append command " > " fifo-name)))
             (output-port (open-remote-input-pipe
                           host
                           (string-append "cat " fifo-name))))
        (slot-set! proc 'input-port  input-port)
        (slot-set! proc 'output-port output-port)
        (slot-set! proc 'fifo-name   fifo-name)))
     (else
      (error "Wrong argument type: " host))))
  (process-state-set! proc 'started))

(define-method (process-stop! (proc <process>))
  (when (process-stopped? proc)
    (error "The process is already stopped" proc))
  (close (process-input-port proc))
  (close (process-output-port proc))
  (slot-set! proc 'fifo-name #f)
  (let ((host (process-host proc)))
    (cond
     ((not host)
      (when (process-fifo-name proc)
        (delete-file (process-fifo-name proc))))
     ((session? (process-host proc))
      (with-ssh (make-node host)
                (delete-file (process-fifo-name proc))))))
  (process-state-set! proc 'stopped))

;;; process.scm ends here.
