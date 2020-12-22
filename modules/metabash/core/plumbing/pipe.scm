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

(define-module (metabash core plumbing pipe)
  #:use-module (oop goops)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:export (<pipe>
            pipe?
            pipe-thread
            pipe-input-port
            pipe-output-port
            pipe-on-disconnect-callback
            pipe-connect!
            pipe-connected?
            pipe-disconnect!
            pipe-close!
            pipe-closed?
            ;; tee
            <tee>
            tee-side-branch-port))



(define (object->naked-string object)
  "Convert an OBJECT to a 'naked' string, that is, without '#<' and '>'."
  (let ((str (object->string object)))
    (substring str 2 (- (string-length str) 1))))


;;; Default callbacks.

(define (%default-on-data-callback! pipe data)
  "Default callback to be called with new DATA is available from a PIPE input
port."
  (slot-set! pipe 'tx (+ (pipe-tx pipe) (bytevector-length data))))

(define (%default-on-disconnect-callback! pipe)
  "Default callback to be called when a PIPE is closed."
  (close (pipe-input-port  pipe))
  (close (pipe-output-port pipe)))


;; This class describes a pipe (akin to Unix pipe) that can connect two ports
;; together by forwarding data from an INPUT-PORT to an OUTPUT-PORT in a
;; separate thread.
(define-class <pipe> ()
  ;; <thread>
  ;;
  ;; The pipe thread.
  (thread      #:accessor     pipe-thread
               #:init-value   #f)

  ;; <port>
  ;;
  ;; The port from which the data will be read.
  (input-port  #:accessor     pipe-input-port
               #:init-value   #f
               #:init-keyword #:input-port)

  ;; <port>
  ;;
  ;; The port the data will be written to.
  (output-port #:accessor     pipe-output-port
               #:init-value   #f
               #:init-keyword #:output-port)

  ;; <number>
  ;;
  ;; The total number of transmitted bytes.
  (tx          #:accessor     pipe-tx
               #:init-value   0)

  ;; <procedure>
  ;;
  ;; This callback is called with the pipe instance and the data as an argument.
  ;; The default callback counts the transmitted data.
  (on-data-callback       #:accessor     pipe-on-data-callback
                          #:init-value   %default-on-data-callback!
                          #:init-keyword #:on-data)

  ;; <procedure>
  ;;
  ;; This callback is called with the pipe instance as an argument.  The default
  ;; callback closes both input and output ports.
  (on-disconnect-callback #:accessor     pipe-on-disconnect-callback
                          #:init-value   %default-on-disconnect-callback!
                          #:init-keyword #:on-disconnect))
(define (pipe? x)
  "Check if X is a <pipe> instance."
  (is-a? x <pipe>))


;; Overloaded methods to display a <port> instance.

(define-method (display (pipe <pipe>) (port <port>))
  (format port "#<pipe [~a] ~a> [~a] tx: ~a ~a>"
          (object->naked-string (pipe-input-port  pipe))
          (if (pipe-connected? pipe)
              "="
              "x")
          (object->naked-string (pipe-output-port pipe))
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
    (when (pipe-closed? pipe)
      (error "One of the ports is closed." input-port output-port))
    (slot-set! pipe 'thread
               (begin-thread
                (let loop ((data (get-bytevector-some input-port)))
                  (unless (or (port-closed? input-port)
                              (port-closed? output-port))
                    (if (eof-object? data)
                        ((pipe-on-disconnect-callback pipe) pipe)
                        (begin
                          ((pipe-on-data-callback pipe) pipe data)
                          (put-bytevector output-port data)
                          (loop (get-bytevector-some input-port))))))))))

;; Predicate.  Check if a PIPE is connected (that is, the pipe thread is
;; running.)
(define-method (pipe-connected? (pipe <pipe>))
  (and (not (equal? (pipe-thread pipe) #f))
       (not (thread-exited? (pipe-thread pipe)))))



(define-generic pipe-disconnect!)

;; Disconnect a PIPE by stopping the pipe thread.  Note that the ports will not
;; be closed.
(define-method (pipe-disconnect! (pipe <pipe>))
  (cancel-thread (pipe-thread pipe))
  (join-thread (pipe-thread pipe))
  (slot-set! pipe 'thread #f))



(define-generic pipe-close!)

;; Close a specified PIPE.
(define-method (pipe-close! (pipe <pipe>))
  (pipe-disconnect! pipe)
  (close (pipe-input-port pipe))
  (close (pipe-output-port pipe)))


(define-generic pipe-closed?)

;; Predicate.  Check if a PIPE is closed.  The PIPE considered as closed if one
;; of the pipe ports is closed.
(define-method (pipe-closed? (pipe <pipe>))
  (or (port-closed? (pipe-input-port pipe))
      (port-closed? (pipe-output-port pipe))))

;;; pipe.scm ends here.
