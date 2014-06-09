(define match-junk
	(lambda (k frag)
		(or (call/cc (lambda (cc)
			(cons frag (lambda () (cc #f)))))
		(and (< 0 k)
			(pair? frag)
			(match-junk (- k 1) (cdr frag))))))

(define match-*
	(lambda (matcher frag)
		(or (call/cc (lambda (cc) 
			(cons frag (lambda () (cc #f))))) ; empty match
		(let ((head (matcher frag)))
			(if head
				(and (not (eq? (car head) frag)) ; check not empty match
					(or (match-* matcher (car head))
						((cdr head)))) ; backtrack
			#f )))))

(define make-matcher
	(lambda (pat)
		(cond
			; pat is a symbol
			((symbol? pat)
				(lambda (frag)
					(and (pair? frag)
						(eq? pat (car frag))
						(call/cc (lambda (cc) (cons (cdr frag) (lambda () (cc #f))))))))
			; (or pat..)
			((eq? 'or (car pat))
				(let make-or-matcher ((pats (cdr pat)))
					(if (null? pats)
						(lambda (frag) (call/cc (lambda (cc) (cons frag (cc #f))))) ; return false			
						(let ((head-matcher (make-matcher (car pats)))
							(tail-matcher (make-or-matcher (cdr pats))))
							(lambda (frag) ; match 1st pat or try the rest
								(let ((head (head-matcher frag)))
									(if head
										(or (call/cc (lambda (cc)
											(cons (car head) (lambda () (cc #f))))) ; return
											(tail-matcher frag)) ; continue and try tail
										(tail-matcher frag)))))))) ; try tail
			; (list pat...)
			((eq? 'list (car pat))
				(let make-list-matcher ((pats (cdr pat)))
					(if (null? pats)
						(lambda (frag) 
							(call/cc (lambda (cc)
								(cons frag (lambda () (cc #f))))))	; return false 
						(let ((head-matcher (make-matcher (car pats)))
							(tail-matcher (make-list-matcher (cdr pats))))
						(lambda (frag)
								(let ((head (head-matcher frag)))
									(if head
										(let ((tail (tail-matcher (car head)))) ; match tail
											(if tail
												tail ; return tail's match
												((cdr head)))) ; backtrack into head's cc
										#f )))))))
			; (junk k)
			((eq? 'junk (car pat))
				(let ((k (cadr pat)))
					(lambda (frag)
						(match-junk k frag))))
			; (* pat)
			((eq? '* (car pat))
				(let ((matcher (make-matcher (cadr pat))))
					(lambda (frag)
						(match-* matcher frag)))))))