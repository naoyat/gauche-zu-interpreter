#!/usr/bin/env gosh
;;
;; Zu interpreter for Gauche. (c)2007 by naoya_t
;;
(use nt.zu)

(define (usage . prog-name)
  (format (current-error-port)
		  "Usage: ~a file\n" *program-name*)
  (exit 2))

(define (main args)
  (if (null? (cdr args))
	  (usage (car args))
	  (let1 data (call-with-input-file (cadr args)
				   (lambda (port) (port->string port)))
		(zu-interpreter data)))
  0)
