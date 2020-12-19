;;; plumber.scm -- Metabash plumbering procedures.

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

;; This file contains procedures description of the pipeline.


;;; Code:

(define-module (metabash core pipeline)
  #:use-module (metabash core plumbing pipe)
  #:use-module (metabash core process)
  #:use-module (oop goops)
  #:use-module (ssh dist node)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:export (<pipeline>
            pipeline?
            pipeline-pipes
            pipeline-processes
            pipeline-output-port
            pipeline-input-port
            pipeline-pretty-print))



(define-class <pipeline> ()
  (pipes     #:accessor     pipeline-pipes
             #:init-value   '()
             #:init-keyword #:pipes)
  (processes #:accessor     pipeline-processes
             #:init-value   '()
             #:init-keyword #:processes))

(define (pipeline? x)
  (is-a? x <pipeline>))

(define-method (display (pipeline <pipeline>) (port <port>))
  (format port "#<pipeline processes: ~a pipes: ~a ~a>"
          (length (pipeline-processes pipeline))
          (length (pipeline-pipes     pipeline))
          (number->string (object-address pipe) 16)))

(define-method (write (pipeline <pipeline>) (port <port>))
  (display pipeline port))

(define-method (display (pipeline <pipeline>))
  (next-method)
  (display pipeline (current-output-port)))

(define-method (write (pipeline <pipeline>))
  (next-method)
  (display pipeline (current-output-port)))

(define-method (pipeline-input-port (pipeline <pipeline>))
  (process-input-port (car (pipeline-processes pipeline))))

(define-method (pipeline-output-port (pipeline <pipeline>))
  (process-output-port (last (pipeline-processes pipeline))))


(define-generic pipeline-pretty-print)

;; Pretty-print the given PIPELINE to the given PORT.
(define-method (pipeline-pretty-print (pipeline <pipeline>) (port <port>))
  (format port ";;; ~a~%" pipeline)
  (let loop ((processes (pipeline-processes pipeline))
             (pipes     (pipeline-pipes pipeline)))
    (format port ";;;   ~a~%" (car processes))
    (unless (null? pipes)
      (format port ";;;     ~a~%" (car pipes)))
    (when (> (length processes) 1)
      (loop (cdr processes)
            (if (null? pipes) pipes (cdr pipes))))))

;; Pretty-print the given PIPELINE to the current output port.
(define-method (pipeline-pretty-print (pipeline <pipeline>))
  (pipeline-pretty-print pipeline (current-output-port)))



