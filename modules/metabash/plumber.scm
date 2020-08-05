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

(set-record-type-printer!
 <pipeline>
 (lambda (pipeline port)
   (format port "#<pipeline processes: ~a pipes: ~a ~a>"
           (length (pipeline-processes pipeline))
           (length (pipeline-pipes     pipeline))
           (number->string (object-address pipe) 16))))

(define (pipeline-input-port pipeline)
  (process-input-port (car (pipeline-processes pipeline))))

(define (pipeline-output-port pipeline)
  (process-output-port (last (pipeline-processes pipeline))))



(define (plumb . spec)
  "Make a pipeline using the SPEC."
  (let loop ((sp        spec)
             (pipes     '())
             (processes '()))
    (if (not (null? sp))
        (let* ((command   (car sp))
               (type      (car command))
               (last-proc (last processes)))
          (case type
            ((local)
             (let ((proc (make-process #f (cadr command))))
               (loop (cdr sp)
                     (if last-proc
                         (append-1 pipes
                                   (make-pipe
                                    (process-output-port last-proc)
                                    (process-input-port  proc)))
                         pipes)
                     (append-1 processes proc))))
            ((remote)
             (let* ((host (cadr  command))
                    (cmd  (caddr command))
                    (proc (make-process host cmd)))
               (loop (cdr sp)
                     (if last-proc
                         (append-1 pipes
                                   (make-pipe
                                    (process-output-port last-proc)
                                    (process-input-port  proc)))
                         pipes)
                     (append-1 processes proc))))))
        (make-pipeline pipes processes))))

;;; plumber.scm ends here.

