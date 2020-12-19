#!/usr/bin/guile \
-L modules -e main -s
!#

;;; kernel-release-diff.scm -- Compare kernel versions of two machines

(use-modules (ssh session)
             (ssh auth)
             (metabash core pipeline)
             (metabash plumber)
             (metabash diff))



(define (print-help-and-exit prog)
  (format (current-error-port)
          "Usage: ~a <host-1> <host-2>~%"
          prog)
  (exit 0))


(define (main args)

  (when (< (length args) 3)
    (print-help-and-exit (car args)))

  (let ((session-1 (make-session #:host (cadr args)))
        (session-2 (make-session #:host (caddr args))))

    (connect! session-1)
    (connect! session-2)

    (userauth-agent! session-1)
    (userauth-agent! session-2)

    (let ((pipeline-1 (M#! remote session-1 "uname -r"))
          (pipeline-2 (M#! remote session-2 "uname -r")))

      (diff pipeline-1 pipeline-2 (current-output-port))

      (while (or (not (port-closed? (pipeline-output-port pipeline-1)))
                 (not (port-closed? (pipeline-output-port pipeline-2))))
        (sleep 1)))))
