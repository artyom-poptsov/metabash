(define-module (metabash pipe)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:export (<pipe>
            pipe-thread
            pipe-input-port
            pipe-output-port
            make-pipe
            pipe-close))

(define-immutable-record-type <pipe>
  (%make-pipe thread input-port output-port)
  pipe?
  (thread      pipe-thread)
  (input-port  pipe-input-port)
  (output-port pipe-output-port))

(set-record-type-printer!
 <pipe>
 (lambda (pipe port)
   (format port "#<pipe thread: ~a in: ~a out: ~a ~a>"
           (pipe-thread pipe)
           (pipe-input-port  pipe)
           (pipe-output-port pipe)
           (number->string (object-address pipe) 16))))

(define (make-pipe input-port output-port)
  (%make-pipe
   (begin-thread
    (let loop ((data (get-bytevector-some input-port)))
      (if (eof-object? data)
          (begin
            (close input-port)
            (close output-port))
          (begin
            (put-bytevector output-port data)
            (loop (get-bytevector-some input-port))))))
   input-port
   output-port))

(define (pipe-close! pipe)
  (close (pipe-input-port pipe))
  (close (pipe-output-port pipe))
  (cancel-thread (pipe-thread pipe))
  (join-thread (pipe-thread pipe)))
