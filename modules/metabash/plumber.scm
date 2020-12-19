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
  #:use-module (metabash core plumbing pipe)
  #:use-module (metabash core process)
  #:use-module (metabash core pipeline)
  #:use-module (oop goops)
  #:use-module (ssh dist node)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:export (plumb
            M#!))


;;; Helper procedures.

(define (last lst)
  "Get the last element of a list LST."
  (and (not (null? lst))
       (car (last-pair lst))))

(define (append-1 lst elem)
  (append lst (list elem)))



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



;; Convenient macro that do the plumbing.
(define-syntax M#!
  (lambda (x)

    (define (-> commands)
      (syntax-case commands (=>)
        ((type command)
         #`((list (quote type) command)))
        ((type command => rest ...)
         (with-syntax (((r ...) (-> #'(rest ...))))
           #'((list (quote type) command) r ...)))
        ((type host command)
         #`((list (quote type) host command)))
        ((type host command => rest ...)
         (with-syntax (((r ...) (-> #'(rest ...))))
           #'((list (quote type) host command) r ...)))))

    (syntax-case x (=>)
      ((_ type command)      #'(plumb (list (quote type) command)))
      ((_ type host command) #'(plumb (list (quote type) host command)))
      ((_ type command => rest ...)
       (with-syntax (((wrapper ...) (-> #'(rest ...))))
         #`(apply plumb (list (list (quote type) command) wrapper ...))))
      ((_ type host command => rest ...)
       (with-syntax (((wrapper ...) (-> #'(rest ...))))
         #`(apply plumb (list (list (quote type) host command) wrapper ...)))))))

;;; plumber.scm ends here.
