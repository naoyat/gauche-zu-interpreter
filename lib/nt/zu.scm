;;;
;;; Zu - ported from yhara's Ruby implementation, by naoya_t
;;;
(define-module nt.zu
  (export
   zu-interpreter
   ))

(select-module nt.zu)

(use nt.textgraph) ;require "nt/textgraph"
(use srfi-1) ;filter

(define (zu-interpreter src)
  (let* ([tg (parse-textgraph src)]
		 [cells [tg'cells]]
		 [links [tg'links]])

	(define (ask msg)
	  (display msg)
	  (display " [y/n]\n> ")
	  (let loop ()
		(case (read-char)
		  ((#\y #\Y) #t)
		  ((#\n #\N) #f)
		  (else (loop)))))
	
	(define (say msg)
	  (print msg)
	  #t)

	(define (eval inst)
	  (case (car inst)
		((ask) (ask (cadr inst)))
		((say) (say (cadr inst)))
		(else #f)))

	(let loop ([i 0])
	  (let* ([cell (ref cells i)]
			 [inst-str (string-append "(" [cell'content] ")")]
			 [inst (read-from-string inst-str)]
			 [ret (eval inst)]
			 [nexts (map cdr (filter (lambda (link) (= i (car link))) links))]
			 )
		(cond ((= 0 (length nexts)) 'done)
			  ((= 1 (length nexts)) (loop (car nexts)))
			  (else
			   (if ret
				   (loop (cadr nexts))
				   (loop (car nexts)))))
		))))

(provide "nt/zu")
;;EOF
