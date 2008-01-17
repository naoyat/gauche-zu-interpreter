;;;
;;; TextGraph - ported from yhara's Ruby implementation, by naoya_t
;;;
(define-module nt.textgraph
  (export
;   make-cell
;   cell=?
;   make-charmap
;   make-parser
   parse-textgraph
   ))

(select-module nt.textgraph)

(define undef-object (if #f #f))

(use srfi-1) ; drop-right
(use srfi-13) ; string-trim
(use gauche.sequence) ; ref

;;
;; Cell
;;
(define (make-cell x y w h raw-content)
  (cond ((or (< x 0) (< y 0)) (error "x or y value too small"))
		((or (< w 2) (< h 2)) (error "w or h value too small"))
		(else (lambda (m)
				(case m
				  ((x) x)
				  ((y) y)
				  ((w) w)
				  ((h) h)
				  ((includes?) (lambda (tx ty) (and (<= x tx (+ x w -1))
													(<= y ty (+ y h -1)))))
				  ((raw-content) raw-content)
				  ((content) (string-trim-both raw-content))
				  ((to-str) (format "[~d ~d ~d ~d \"~a\"]" x y w h (string-trim-both raw-content)))
				  (else undef-object) ))
			  )))

(define (cell=? c1 c2)
  (and (= [c1'x] [c2'x])
	   (= [c1'y] [c2'y])
	   (= [c1'w] [c2'w])
	   (= [c1'h] [c2'h])
	   (string=? [c1'raw-content] [c2'raw-content])))

;;
;; CharMap
;;
(define (string->lines str)
  (drop-right (string-split str #[\r\n])
			  1))

(define (make-charmap str)
  (define (partition str)
	(map string->list
		 (string->lines str)))

  (let* ([data (partition str)]
		 [width (apply max (map length data))]
		 [height (length data)]
		 )

	(define (x-in-range? x)
	  (and (<= 0 x) (< x width)))
	(define (y-in-range? y)
	  (and (<= 0 y) (< y height)))

	(define (char-at x y) ; [](x,y)
;	  (if (and (x-in-range? x) (y-in-range? y))
;		  (ref (ref data y) x)
;		  undef-object))
	  (if (y-in-range? y)
		  (let1 line (ref data y)
			(if (and (<= 0 x) (< x (length line)))
				(ref line x)
				undef-object))
		  undef-object))

	(define (map-char proc) ; takes (lambda (x y char) ...)
	  (map-with-index (lambda (y line)
						(map-with-index (lambda (x char)
										  (apply proc (list x y char))
										  )
										line)
						)
					  data))

	(define (each-char proc) ; takes (lambda (x y char) ...)
	  (for-each-with-index (lambda (y line)
							 (for-each-with-index (lambda (x char)
													(apply proc (list x y char))
													)
												  line)
							 )
						   data))

	(lambda (m)
	  (case m
		((raw-data) data)
		((width) width)
		((height) height)
		((x-in-range?) x-in-range?)
		((y-in-range?) y-in-range?)
		((char-at) char-at)
		((map-char) map-char)
		((each-char) each-char)
		(else undef-object)))
	))

;;
;; Graph
;;
(define (make-graph cells links)
;  (format #t "(make-graph ~a ~a)\n" cells links)
  (define (dump)
	(for-each-with-index
	 (lambda (i cell) (format #t "~d) ~a\n" i [cell'to-str]))
	 cells)
	(for-each
	 (lambda (link) (format #t "~d --> ~d\n" (car link) (cdr link)))
	 links))

  (lambda (m)
	(case m
	  ((cells) cells)
	  ((links) links)
	  ((dump) (dump))
	  (else undef-object))))

(define (make-coord x y)
  (lambda (m)
	(case m
	  ((x) x)
	  ((y) y)
	  ((to-str) (format "(~d,~d)" x y))
	  (else undef-object))))

(define (coord=? co1 co2)
  (and (= [co1'x] [co2'x])
	   (= [co1'y] [co2'y])))

;;
;; Parser
;;
(define (make-parser str)
  (let1 charmap (make-charmap str)
	
	(define (collect-chars char)
	  (remove! null?
			   (apply append!
					  ([charmap'map-char] (lambda (x y c)
											(if (eq? c char) (make-coord x y) '())
											)
					   ))))

	(define (find-horizontal-end x y dir goal-ch)
	  (let1 dx (if dir 1 -1)
		(let loop ([tx (+ x dx)] [ty y])
		  (cond ((not ([charmap'x-in-range?] tx)) #f)
				((eq? goal-ch ([charmap'char-at] tx ty)) (make-coord tx ty))
				(else (loop (+ tx dx) ty))))
		))
	(define (find-vertical-end x y dir goal-ch)
	  (let1 dy (if dir 1 -1)
		(let loop ([tx x] [ty (+ y dy)])
		  (cond ((not ([charmap'y-in-range?] ty)) #f)
				((eq? goal-ch ([charmap'char-at] tx ty)) (make-coord tx ty))
				(else (loop tx (+ ty dy)))))
		))

	(define (cell? x y)
	  (if (and (memq ([charmap'char-at] (+ x 1) y) '(#\- #\v #\+))
			   (memq ([charmap'char-at] x (+ y 1)) '(#\| #\> #\+)))
		  (let* ([rt (find-horizontal-end x y #t #\*)]
				 [lb (find-vertical-end x y #t #\*)]
				 [lb-r (find-horizontal-end x [lb'y] #t #\*)]
				 [rt-b (find-vertical-end [rt'x] y #t #\*)])
			(if (and rt lb lb-r rt-b)
				(if (coord=? lb-r rt-b)
					(make-coord (+ (- [rt'x] x) 1) (+ (- [lb'y] y) 1))
					#f)
				#f) )
		  #f))

	(define (get-str x y w h)
	  (string-join (map (lambda (ty)
						  (list->string (map (lambda (tx)
											   ([charmap'char-at] tx ty))
											 (iota (- w 2) (+ x 1)) )))
						(iota (- h 2) (+ y 1)) )))

	(define (collect-cells)
	  (remove! null?
			   (map (lambda (coord)
					  (let ([x (coord'x)]
							[y (coord'y)])
						(let1 wh (cell? x y)
						  (if wh
							  (let ([w (wh'x)]
									[h (wh'y)])
								(make-cell x y w h (get-str x y w h)))
							  '())
						  )))
					(collect-chars #\*)
					)))

	(define (find-links cells)
	  (map (lambda (coord)
			 (let ([x (coord'x)]
				   [y (coord'y)])
			   (let1 p (or (find-horizontal-end x y #t #\>)
						   (find-horizontal-end x y #f #\<)
						   (find-vertical-end x y #t #\v)
						   (find-vertical-end x y #f #\^))
				 (cons (cell-at cells x y)
					   (cell-at cells [p'x] [p'y]))
				 )))
		   (collect-chars #\+)))

	(define (cell-at cells x y)
	  (find-index
	   (lambda (cell) ([cell'includes?] x y))
	   cells))

	(define (get-direction x y)
	  (cond ((memq ([charmap'char-at] (+ x 1) y) '(#\- #\>)) (list 1 0 #\>))
			((memq ([charmap'char-at] x (+ y 1)) '(#\| #\v)) (list 0 1 #\v))
			((memq ([charmap'char-at] (- x 1) y) '(#\- #\<)) (list -1 0 #\<))
			((memq ([charmap'char-at] x (- y 1)) '(#\| #\^)) (list 0 -1 #\^))
			(else (error "can't decide direction"))))

	(define (parse)
	  (let* ([cells (collect-cells)]
			 [links (find-links cells)])
		(make-graph cells links)))

	(lambda (m)
	  (case m
		((collect-chars) collect-chars)
		((collect-cells) (collect-cells))
		((parse) (parse))
;		((dump) (dump))
		(else undef-object)))
	))

(define (parse-textgraph str)
  [(make-parser str)'parse])

(provide "nt/textgraph")
;;EOF
