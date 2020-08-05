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
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:export (<pipe>
            pipe-thread
            pipe-input-port
            pipe-output-port
            make-pipe
            pipe-close))

(define-immutable-record-type <pipe>
  (%make-pipe thread input-port output-port)
  pipe?
  (thread      pipe-thread)
  (input-port  pipe-input-port)
  (output-port pipe-output-port))

(set-record-type-printer!
 <pipe>
 (lambda (pipe port)
   (format port "#<pipe thread: ~a in: ~a out: ~a ~a>"
           (pipe-thread pipe)
           (pipe-input-port  pipe)
           (pipe-output-port pipe)
           (number->string (object-address pipe) 16))))



(define (make-pipe input-port output-port)
  "Make a new pipe that connects INPUT-PORT and OUTPUT-PORT."
  (when (or (port-closed? input-port) (port-closed? output-port))
    (error "One of the ports is closed." input-port output-port))
  (%make-pipe
   (begin-thread
    (let loop ((data (get-bytevector-some input-port)))
      (unless (or (port-closed? input-port) (port-closed? output-port))
        (if (eof-object? data)
            (begin
              (close input-port)
              (close output-port))
            (begin
              (put-bytevector output-port data)
              (loop (get-bytevector-some input-port)))))))
   input-port
   output-port))

(define (pipe-close! pipe)
  "Close a specified PIPE."
  (close (pipe-input-port pipe))
  (close (pipe-output-port pipe))
  (cancel-thread (pipe-thread pipe))
  (join-thread (pipe-thread pipe)))

;;; pipe.scm ends here.
