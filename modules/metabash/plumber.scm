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

;; This file contains procedures for connecting processes in a pipeline, namely
;; the 'plumb' procedure.


;;; Code:

(define-module (metabash plumber)
  #:use-module (metabash pipe)
  #:use-module (metabash process)
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
            plumb
            M#!))


;;; Helper procedures.

(define (last lst)
  "Get the last element of a list LST."
  (and (not (null? lst))
       (car (last-pair lst))))

(define (append-1 lst elem)
  (append lst (list elem)))



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



(define-method (run-local (command <list>))
  "Run local COMMAND."
  (let ((proc (make <process> #:command (cadr command))))
    (process-start! proc)
    proc))

(define-method (run-remote (command <list>))
  "Run a remote command using Guile-SSH."
  (let* ((host (cadr  command))
         (cmd  (caddr command))
         (proc (make <process> #:host host #:command cmd)))
    (process-start! proc)
    proc))

(define-method (run-guile (command <list>))
  "Run a GNU Guile command in a local or a remote process."
  (let* ((host (cadr command))
         (proc (make <process> #:host host #:command "sh -c guile -q --")))
    (process-start! proc)
    (write-line ",option prompt \"\"" (process-input-port proc))
    (rrepl-skip-to-prompt (process-output-port proc))
    (write (caddr command) (process-input-port proc))
    (newline (process-input-port proc))
    (force-output (process-input-port proc))
    proc))

(define-method (append-process (process-list <list>) (pipe-list <list>) process)
  "Append a new PROCESS to the PROCESS-LIST.  If the PROCESS-LIST is not empty,
connect the last process from the list with the new PROCESS.

Return two values: updated PROCESS-LIST with the new PROCESS and updated
PIPE-LIST."
  (let ((last-proc (last process-list)))
    (if last-proc
        (let ((pipe (make <pipe>
                      #:input-port  (process-output-port last-proc)
                      #:output-port (process-input-port  process))))
          (pipe-connect! pipe)
          (values (append-1 process-list process)
                  (append-1 pipe-list    pipe)))
        (values (append-1 process-list process)
                pipe-list))))

(define (plumb . spec)
  "Make a pipeline using the SPEC."
  (let loop ((sp        spec)
             (processes '())
             (pipes     '()))
    (if (not (null? sp))
        (let* ((command (car sp))
               (type    (car command)))
          (case type
            ((local)
             (let-values (((process-list pipe-list)
                           (append-process processes pipes (run-local command))))
               (loop (cdr sp) process-list pipe-list)))
            ((remote)
             (let-values (((process-list pipe-list)
                          (append-process processes pipes (run-remote command))))
               (loop (cdr sp) process-list pipe-list)))
            ((guile)
             (let-values (((process-list pipe-list)
                           (append-process processes pipes (run-guile command))))
               (loop (cdr sp) process-list pipe-list)))))
        (make <pipeline> #:pipes pipes #:processes processes))))

(define-syntax M#!
  (syntax-rules (=>)
    ((_ type command)
     (list (quote type) command))
    ((_ => type command)
     `(,(list (quote type) command)))
    ((_ => type command rest ...)
     `(,(list (quote type) command) ,@(M#! rest ...)))
    ((_ type command => rest ...)
     (apply plumb `(,(list (quote type) command) ,@(M#! => rest ...))))))

;;; plumber.scm ends here.
