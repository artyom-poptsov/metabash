#!/usr/bin/guile \
-L modules --no-auto-compile -e main -s
!#

;;; group-grep.scm -- A program to grep '/etc/group' on a remote host.

(use-modules (oop goops)
             (ssh session)
             (ssh auth)
             (metabash plumber)
             (metabash core plumbing pipe))

(define (print-help-and-exit prog)
  (format #t "Usage: ~a <host> <query>~%" prog)
  (exit 0))

(define (main args)

  (when (< (length args) 3)
    (print-help-and-exit (car args)))

  (let ((session (make-session #:host (cadr args)))
        (user    (caddr args)))
    (connect! session)
    (userauth-agent! session)
    (let ((pipeline (M#! remote session "cat /etc/group"
                         => local "sort"
                         => local (format #f "grep '~a'" user))))

      (pipeline-pretty-print pipeline)

      (let ((pipe (make <pipe>
                    #:input-port    (pipeline-output-port pipeline)
                    #:output-port   (current-output-port)
                    #:on-disconnect (lambda (pipe)
                                      ;; (format (current-error-port)
                                      ;;         "Closing pipe: ~a ...~%"
                                      ;;         pipe)
                                      (pipe-close! pipe)
                                      (disconnect! session)))))
        (pipe-connect! pipe)
        (while (pipe-connected? pipe)
          (sleep 1))))))
