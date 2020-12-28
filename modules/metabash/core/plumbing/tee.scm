;;; tee.scm -- Tee implementation for Metabash.

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

;; This file contains implementation of a tee that can connect an input port to
;; two output ports effectively copying the output to two ports simultaneously ,
;; akin to the Unix 'tee' command.


;;; Code:

(define-module (metabash core plumbing tee)
  #:use-module (oop goops)
  #:use-module (metabash core plumbing pipe)
  #:export (<tee>
            tee-side-branch-port))



;;; Tee implementation.

;; This class describes a tee that can send a data from an INPUT-PORT to both an
;; OUTPUT-PORT and a SIDE-BRANCH-PORT.
(define-class <tee> (<pipe>)
  (side-branch-port #:accessor     tee-side-branch-port
                    #:init-value   #f
                    #:init-keyword #:side-branch-port))


;;; Default tee callbacks.

(define (%default-tee-on-disconnect-callback! tee)
  "Default callback to be called when a PIPE is closed."
  (close (pipe-input-port      tee))
  (close (pipe-output-port     tee))
  (close (tee-side-branch-port tee)))


;;; The <tee> constructor.

(define-method (initialize (tee <tee>) initargs)
  (next-method)
  (unless (memq #:on-disconnect initargs)
    (slot-set! tee 'on-disconnect-callback
               %default-tee-on-disconnect-callback!)))


;; Overloaded methods to display a <tee> instance.

;; TODO: Make the format less cumbersome.
(define-method (display (tee <tee>) (port <port>))
  (format port "#<tee [~a] ~a> [~a], [~a] tx: ~a ~a>"
          (object->naked-string (pipe-input-port  tee))
          (if (pipe-thread tee)
              "="
              "x")
          (object->naked-string (tee-side-branch-port tee))
          (object->naked-string (pipe-output-port tee))
          (pipe-tx tee)
          (number->string (object-address tee) 16)))

(define-method (write (tee <tee>) (port <tee>))
  (display tee port))

(define-method (display (tee <tee>))
  (next-method)
  (display tee (current-output-port)))

(define-method (write (tee <tee>))
  (next-method)
  (display tee (current-output-port)))



;; Redirect data from INPUT-PORT to OUTPUT-PORT and BRANCH-OUTPUT-PORT.
(define-method (pipe-connect! (tee <tee>))
  (let ((input-port  (pipe-input-port tee))
        (output-port (pipe-output-port tee))
        (branch-port (tee-side-branch-port tee)))
    (when (pipe-closed? tee)
      (error "One of the ports is closed."
             input-port output-port branch-port))
    (slot-set! tee 'thread
               (begin-thread
                (let loop ((data (get-bytevector-some input-port)))
                  (unless (eof-object? data)
                    (slot-set! tee 'tx (+ (pipe-tx tee) (bytevector-length data)))
                    (put-bytevector branch-port data)
                    (put-bytevector output-port data)
                    (loop (get-bytevector-some input-port))))))))



;; Close a specified TEE by stopping the thread and closing the ports.
(define-method (pipe-close! (tee <tee>))
  (pipe-disconnect! tee)
  (close (pipe-input-port tee))
  (close (tee-side-branch-port tee))
  (close (pipe-output-port tee)))

;; Check if a TEE is closed.  The tee is considered as closed if any of its
;; ports is closed.
(define-method (pipe-closed? (tee <tee>))
  (or (port-closed? (pipe-input-port tee))
      (port-closed? (pipe-output-port tee))
      (port-closed? (tee-side-branch-port tee))))

;;; tee.scm ends here.

