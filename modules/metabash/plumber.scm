(define-module (metabash plumber)
  #:use-module (metabash pipe)
  #:use-module (metabash process)
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



(define (run-local command)
  (make-process #f (cadr command)))

(define (run-remote command)
  (let ((host (cadr  command))
        (cmd  (caddr command)))
    (make-process host cmd)))

(define (run-guile command)
  (let* ((host (cadr command))
         (proc (make-process host "sh -c guile -q --")))
    (write-line ",option prompt \"\"" (process-input-port proc))
    (rrepl-skip-to-prompt (process-output-port proc))
    (write (caddr command) (process-input-port proc))
    (newline (process-input-port proc))
    (force-output (process-input-port proc))
    proc))

(define (append-process process-list pipe-list process)
  (let ((last-proc (last process-list)))
    (if last-proc
        (values (append-1 process-list process)
                (append-1 pipe-list    (make-pipe (process-output-port last-proc)
                                                  (process-input-port  process))))
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
        (make-pipeline pipes processes))))

;;; plumber.scm ends here.

