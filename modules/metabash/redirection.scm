;;; redirection.scm -- Output redirection procedures.

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

;; This file contains implementation redirection procedures.


;;; Code:

(define-module (metabash redirection)
  #:use-module (oop goops)
  #:use-module (metabash core plumbing pipe)
  #:use-module (metabash core process)
  #:use-module (metabash plumber)
  #:export (M> M>>))

;;; Redirections.

(define-generic M>)

(define-method (%redirect (input-port <port>) (file-name <string>) (mode <string>))
  (let* ((file-port (open-file file-name mode))
         (pipe      (make <pipe>
                      #:input-port  input-port
                      #:output-port file-port)))
    (pipe-connect! pipe)
    pipe))

;; Redirect output from an INPUT-PORT to a FILE-NAME. The contents of the file
;; will be overwritten.
(define-method (M> (input-port <port>) (file-name <string>))
  (%redirect input-port file-name "wb"))

(define-method (M> (process <process>) (file-name <string>))
  (M> (process-output-port process) file-name))

(define-method (M> (pipeline <pipeline>) (file-name <string>))
  (M> (pipeline-output-port pipeline) file-name))



(define-generic M>>)

;; Redirect output from an INPUT-PORT to a FILE-NAME. The data will be appended
;; to the end of the file.
(define-method (M>> (input-port <port>) (file-name <string>))
  (%redirect input-port file-name "ab"))

(define-method (M>> (process <process>) (file-name <string>))
  (M>> (process-output-port process) file-name))

(define-method (M>> (pipeline <pipeline>) (file-name <string>))
  (M>> (pipeline-output-port pipeline) file-name))
