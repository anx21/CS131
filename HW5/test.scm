(define frag0 '())
(define frag1 '(a t g c t a))
;
; Scheme does not care about the newlines in the definition of frag2.
; From Scheme's point of view, they are merely extra white space that
; is ignored.  The newlines are present only to help humans understand
; how the patterns defined below are matched against frag2.
(define frag2 '(c c c g a t a a a a a a g t g t c g t
                a
                a g t a t a t g g a t a
                t a
                a g t a t a t g g a t a
                c g a t c c c t c g a t c t a))

; Most of the uses of "list" in the following pattern definitions
; are as a symbol that is part of a pattern, not as a procedure.
; However, there are two exceptions, one when defining pat3
; and the other when defining pat4.
(define pat1 '(list a t g c))
(define pat2 '(or
               (list a g t a t a t g g a t a)
               (list g t a g g c c g t)
               (list c c c g a t a a a a a a g t g t c g t)
               (list c g a t c c c (junk 1) c g a t c t a)))
(define pat3 (list 'list pat2 '(junk 2)))
(define pat4 (list '* pat3))
(define my_pat '(junk 2))

; For each pattern defined above, use "make-matcher" to create a
; matcher that matches the pattern.
(define matcher1 (make-matcher pat1))
(define matcher2 (make-matcher pat2))
(define matcher3 (make-matcher pat3))
(define matcher4 (make-matcher pat4))


; Return the first solution acceptable to ACCEPT.
(define (acceptable-match matcher frag accept)
  (let ((r (matcher frag)))	; Added accept parameter to code
    (and r
         (or (accept (car r))
             ((cdr r))))))

; Return the first match.
(define (first-match matcher frag)
  (acceptable-match matcher frag (lambda (frag1) frag1)))

; Return true if the matcher matches all of the fragment.
(define (match-all? matcher frag)
  (acceptable-match matcher frag null?))

; Output all solutions.
(define (write-then-fail matcher frag)
  (let ((m (matcher frag)))
     (if m
         (begin
	   (write (car m))
           (newline)
           ((cdr m)))
         (void))))

; Some test cases.
(display "Test 1 #f: ") (first-match matcher1 frag0) ; ⇒ #f

; A match must always match an entire prefix of a fragment.
; So, even though matcher1 finds a match in frag1,
; it does not find the match in (cons 'a frag1).
(display "Test 2 (t a): ") (first-match matcher1 frag1) ; ⇒ (t a)
(display "Test 3 #f: ") (first-match matcher1 (cons 'a frag1)) ; ⇒ #f

(display "Test 4 #f: ") (first-match matcher2 frag1) ; ⇒ #f
(first-match matcher2 frag2) ; ⇒ (a
;                                 a g t a t a t g g a t a
;                                 t a
;                                 a g t a t a t g g a t a
;                                 c g a t c c c t c g a t c t a)

; These matcher calls match the same prefix,
; so they return unmatched suffixes that are eq?.
(display "Test 5 #t: ") (eq? (first-match matcher2 frag2)
     (first-match matcher3 frag2)) ; ⇒ #t

; matcher4 is lazy: it matches the empty fragment first,
; but you can force it to backtrack by insisting on progress.
(display "Test 6 #t: ") (eq? (first-match matcher4 frag2)
     frag2) ; ⇒ #t

(display "Test 7 #t: ") (eq? (first-match matcher2 frag2)
     (acceptable-match matcher4
           frag2
           (lambda (frag) (if (eq? frag frag2) #f frag))))
; ⇒ #t

; Here null? is being used as an acceptor.
; It accepts only the empty unmatched suffix,
; so it forces matcher4 to backtrack until all of frag2 is matched.
(display "Test 8 #f: ") (match-all? matcher1 frag2) ; ⇒ #f
(display "Test 9 #t: ") (match-all? matcher4 frag2) ; ⇒ #t

; Junk test - GOOD
(define my_pat1 '(junk 2))
(define my_matcher1 (make-matcher my_pat1))
(define my_test1 (my_matcher1 frag1))
(display "my_test1 (junk): ") my_test1

; Symbol test - GOOD?
(define my_pat2 'a)	; match nucleotide a
(define my_matcher2 (make-matcher my_pat2))
(define my_test2 (my_matcher2 '(a g t t)))
(display "my_test2 (symbol): ") my_test2

; Or test
(define my_pat3 '(or (list a) (list a b) (list a b c)))
(define my_matcher3 (make-matcher my_pat3))
(define my_test3 (my_matcher3 '(a b c d e)))
(display "my_test3 (or): ") my_test3

(define my_pat4 '(or a b c))
(define my_matcher4 (make-matcher my_pat4))
(define my_test4 (my_matcher4 '(d a a)))
(display "my_test4 (or): ") my_test4

; List test
(define my_pat5 '(list a g))
(define my_matcher5 (make-matcher my_pat5))
(define my_test5 (my_matcher5 '(a g t t)))

