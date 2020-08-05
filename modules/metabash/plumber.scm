(define-module (metabash plumber)
  #:use-module (metabash pipe)
  #:use-module (metabash process)
  #:use-module (srfi srfi-9 gnu)
  #:export (<pipeline>
            pipeline?
            pipeline-pipes
            pipeline-processes
            pipeline-output-port
            pipeline-input-port
            plumb))


;;; Helper procedures.

(define (last lst)
  "Get the last element of a list LST."
  (and (not (null? lst))
       (car (last-pair lst))))

(define (append-1 lst elem)
  (append lst (list elem)))



(define-immutable-record-type <pipeline>
  (make-pipeline pipes processes)
  pipeline?
  (pipes     pipeline-pipes)
  (processes pipeline-processes))

(define (pipeline-input-port pipeline)
  (process-input-port (car (pipeline-processes pipeline))))

(define (pipeline-output-port pipeline)
  (process-output-port (last (pipeline-processes pipeline))))



(define (plumb . spec)
  "Make a pipeline using the SPEC."
  (let loop ((commands  spec)
             (pipes     '())
             (processes '()))
      (if (not (null? commands))
          (let* ((cmd       (car commands))
                 (host      (car cmd))
                 (proc      (make-process host (cadr cmd)))
                 (last-proc (last processes)))
            (loop (cdr commands)
                  (if last-proc
                      (append-1 pipes
                                (make-pipe (process-output-port last-proc)
                                           (process-input-port  proc)))
                      pipes)
                  (append-1 processes proc)))
          (make-pipeline pipes processes))))

;;; plumber.scm ends here.

