;;; pipe.scm -- Pipe implementation for Metabash.

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

;; This file contains implementation of pipes that can connect two ports
;; together akin to Unix pipes.


;;; Code:

(define-module (metabash pipe)
  #:use-module (oop goops)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:export (<pipe>
            pipe?
            pipe-thread
            pipe-input-port
            pipe-output-port
            pipe-connect!
            pipe-disconnect!
            pipe-close!
            ;; tee
            <tee>
            tee-side-branch-port))

(define-class <pipe> ()
  ;; <thread>
  (thread      #:accessor     pipe-thread
               #:init-value   #f)
  ;; <port>
  (input-port  #:accessor     pipe-input-port
               #:init-value   #f
               #:init-keyword #:input-port)
  ;; <port>
  (output-port #:accessor     pipe-output-port
               #:init-value   #f
               #:init-keyword #:output-port)
  ;; <number>
  (tx          #:accessor     pipe-tx
               #:init-value   0))

(define (pipe? x)
  "Check if X is a <pipe> instance."
  (is-a? x <pipe>))


(define-method (display (pipe <pipe>) (port <port>))
  (format port "#<pipe [~a]=~a=[~a] tx: ~a ~a>"
          (if (pipe-thread pipe)
              "="
              "x")
          (pipe-input-port  pipe)
          (pipe-output-port pipe)
          (pipe-tx pipe)
          (number->string (object-address pipe) 16)))

(define-method (write (pipe <pipe>) (port <port>))
  (display pipe port))

(define-method (display (pipe <pipe>))
  (next-method)
  (display pipe (current-output-port)))

(define-method (write (pipe <pipe>))
  (next-method)
  (display pipe (current-output-port)))



(define-generic pipe-connect!)

;; Make a new pipe that connects INPUT-PORT and OUTPUT-PORT.
(define-method (pipe-connect! (pipe <pipe>))
  (let ((input-port  (pipe-input-port  pipe))
        (output-port (pipe-output-port pipe)))
    (when (or (port-closed? input-port)
              (port-closed? output-port))
      (error "One of the ports is closed." input-port output-port))
    (slot-set! pipe 'thread
               (begin-thread
                (let loop ((data (get-bytevector-some input-port)))
                  (unless (or (port-closed? input-port)
                              (port-closed? output-port))
                    (if (eof-object? data)
                        (begin
                          (close input-port)
                          (close output-port))
                        (begin
                          (slot-set! pipe 'tx (+ (pipe-tx pipe) (bytevector-length data)))
                          (put-bytevector output-port data)
                          (loop (get-bytevector-some input-port))))))))))

(define-generic pipe-disconnect!)
(define-method (pipe-disconnect! (pipe <pipe>))
  (cancel-thread (pipe-thread pipe))
  (join-thread (pipe-thread pipe))
  (slot-set! pipe 'thread #f))

(define-generic pipe-close!)

;; Close a specified PIPE.
(define-method (pipe-close! (pipe <pipe>))
  (pipe-disconnect! pipe)
  (close (pipe-input-port pipe))
  (close (pipe-output-port pipe))
  (cancel-thread (pipe-thread pipe)))


;;; Tee implementation.

(define-class <tee> (<pipe>)
  (side-branch-port #:accessor     tee-side-branch-port
                    #:init-value   #f
                    #:init-keyword #:side-branch-port))

(define-method (display (tee <tee>) (port <port>))
  (format port "#<tee [~a]=~a=[~a]=~a=[~a] tx: ~a ~a>"
          (pipe-input-port  tee)
          (if (pipe-thread tee)
              "="
              "x")
          (tee-side-branch-port tee)
          (if (pipe-thread tee)
              "="
              "x")
          (pipe-output-port tee)
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
    (when (or (port-closed? input-port)
              (port-closed? output-port)
              (port-closed? branch-port))
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

;; Close a specified TEE."
(define-method (pipe-close! (tee <tee>))
  (pipe-disconnect! tee)
  (close (pipe-input-port tee))
  (close (tee-side-branch-port tee))
  (close (pipe-output-port tee)))

;;; pipe.scm ends here.
