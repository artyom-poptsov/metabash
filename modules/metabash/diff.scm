;;; diff.scm -- Metabash diffutils.

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

;; This file contains procedures that allow to check differences between
;; outputs.


;;; Code:

(define-module (metabash diff)
  #:use-module (oop goops)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (metabash pipe)
  #:use-module (metabash process)
  #:use-module (metabash redirection)
  #:export (diff))

(define (mktempdir template)
  "Make a temporary directory with the name as specified by a TEMPLATE."
  (read-line (open-input-pipe (string-append "mktemp -d '" template "'"))))

(define (make-diff-command f1 f2)
  (format #f "diff '~a' '~a'" f1 f2))



;; Read and compare an input from an INPUT-PORT-1 with an input from
;; INPUT-PORT-2, print differences in Unix diff format to an OUTPUT-PORT.
(define-method (diff (input-port-1 <port>) (input-port-2 <port>) (output-port <port>))
  (let* ((template  "/tmp/metabash-XXXXXX")
         (directory (mktempdir template))
         (file1     (string-append directory "/input-1"))
         (file2     (string-append directory "/input-2"))
         (pipe1     (M> input-port-1 file1))
         (pipe2     (M> input-port-1 file2)))

    (while (and (not (port-closed? input-port-1))
                (not (port-closed? input-port-2)))
           (sleep 1))

    (pipe-close! pipe1)
    (pipe-close! pipe2)

    (let ((process (make <process> #:command (make-diff-command file1 file2))))

      (process-start! process)

      (let ((output-pipe (make <pipe>
                           #:input-port  (process-output-port process)
                           #:output-port output-port)))
        (pipe-connect! output-pipe)

        (while (pipe-connected? output-pipe)
               (sleep 1))

        (delete-file file1)
        (delete-file file2)
        (rmdir directory)

        (pipe-close! output-pipe)))))


;;; diff.scm ends here.
