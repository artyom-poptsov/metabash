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

(define-module (metabash process)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ssh popen)
  #:use-module (ssh session)
  #:use-module (ssh dist)
  #:export (<process>
            process-input-port
            process-output-port
            process-host
            process-command
            process-fifo-name
            make-process))

(define-immutable-record-type <process>
  (%make-process host command fifo-name input-port output-port)
  process?
  (host        process-host)
  (command     process-command)
  (fifo-name   process-fifo-name)
  (input-port  process-input-port)
  (output-port process-output-port))

(define (make-remote-fifo session)
  "Make a remote FIFO using a SSH session.  Return the FIFO name."
  (read-line
   (open-remote-input-pipe
    session
    "export NAME=$(mktemp --dry-run) &&  mkfifo ${NAME} && echo ${NAME}")))

(define (make-process host command)
  (cond
   ((not host)
    (let ((fifo-name (tmpnam)))
      (system (string-append "mkfifo " fifo-name))
      (let ((input-port  (open-output-pipe
                          (string-append command " > " fifo-name)))
            (output-port (open-input-pipe
                          (string-append "cat " fifo-name))))
        (%make-process host command fifo-name
                       input-port output-port))))
   ((session? host)
    (let* ((fifo-name   (make-remote-fifo host))
           (input-port  (open-remote-output-pipe
                         host
                         (string-append command " > " fifo-name)))
           (output-port (open-remote-input-pipe
                         host
                         (string-append "cat " fifo-name))))
      (%make-process host command fifo-name
                     input-port output-port)))
   (else
    (error "Wrong argument type: " host))))

(define (process-stop! process)
  (close (process-input-port process))
  (close (process-output-port process))
  (let ((host (process-host process)))
    (cond
     ((not host)
      (delete-file (process-fifo-name process)))
     ((session? (process-host process))
      (with-ssh (make-node host)
                (delete-file (process-fifo-name process)))))))

;;; process.scm ends here.
