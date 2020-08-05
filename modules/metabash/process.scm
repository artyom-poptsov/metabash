(define-module (metabash process)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 popen)
  #:use-module (ssh popen)
  #:use-module (ssh session)
  #:use-module (ssh dist)
  #:export (<process>
            process-input-port
            process-output-port
            process-host
            process-command
            process-fifo-name
            make-process))

(define-immutable-record-type <process>
  (%make-process host command fifo-name input-port output-port)
  process?
  (host        process-host)
  (command     process-command)
  (fifo-name   process-fifo-name)
  (input-port  process-input-port)
  (output-port process-output-port))

(define (make-process host command)
  (cond
   ((not host)
    (let ((fifo-name (tmpnam)))
      (system (string-append "mkfifo " fifo-name))
      (let ((input-port  (open-output-pipe
                          (string-append command " > " fifo-name)))
            (output-port (open-input-pipe
                          (string-append "cat " fifo-name))))
        (%make-process host command fifo-name
                       input-port output-port))))
   ((session? host)
    (let* ((fifo-name (with-ssh (make-node host)
                        (let ((name (tmpnam)))
                          (system (string-append "mkfifo " name))
                          name)))
           (input-port  (open-remote-output-pipe
                         host
                         (string-append command " > " fifo-name)))
           (output-port (open-remote-input-pipe
                         host
                         (string-append "cat " fifo-name))))
      (%make-process host command fifo-name
                     input-port output-port)))
   (else
    (error "Wrong argument type: " host))))

(define (process-stop! process)
  (close (process-input-port process))
  (close (process-output-port process))
  (let ((host (process-host process)))
    (cond
     ((not host)
      (delete-file (process-fifo-name process)))
     ((session? (process-host process))
      (with-ssh (make-node host)
                (delete-file (process-fifo-name process)))))))

