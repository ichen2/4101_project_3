(define (eqv? x y)
  (if (and (number? x) (number? y)) (b= x y)
      (eq? x y)))

(define (b> x y) (b< y x))
(define (b<= x y) (or (b= x y) (b< x y)))
(define (b>= x y) (or (b= x y) (b> x y)))

(define (= x y . l)
  (if (null? l) (b= x y)
      (and (b= x y) (apply = (cons y l)))))

(define (< x y . l)
  (if (null? l) (b< x y)
      (and (b< x y) (apply < (cons y l)))))

(define (> x y . l)
  (if (null? l) (b> x y)
      (and (b> x y) (apply > (cons y l)))))

(define (<= x y . l)
  (if (null? l) (b<= x y)
      (and (b<= x y) (apply <= (cons y l)))))

(define (>= x y . l)
  (if (null? l) (b>= x y)
      (and (b>= x y) (apply >= (cons y l)))))

(define (zero? x) (b= x 0))
(define (positive? x) (b< 0 x))
(define (negative? x) (b< x 0))

(define (+ . l)
  (if (null? l) 0
      (b+ (car l) (apply + (cdr l)))))

(define (/ . l)
        (if (= (length l) 0) 
                (type-error "cannot divide by nothing" 1)
                (if (= (length l) 1)
                        (type-error "cannot divide by one element" 1)
                        (if (= (length l) 2) 
                                (b/ (car l) (car (cdr l))) 
                                (b/ (car l) (apply * (cdr l)))
                        )
                )
        )
)

(define (- . l)
        (if (null? l) 
                0
                (if (= (length l) 1)
                        (car l)
                        (b+ (car l) (apply r- (cdr l)))
                )
        )
)

; recursive function used to subtract by adding the negative version of a number
(define (r- . l)
        (if (null? l) 
                0
                (let ((x (b* (car l) -1)))
                        (if (= (length l) 1)
                                x
                                (b+ x (apply r- (cdr l)))
                        )
                )
        )
)

(define (* . l)
  (if (null? l) 1
      (b* (car l) (apply * (cdr l)))))

(define (not b) (if b #f #t))
(define (and x y) (if x y #f))
(define (or x y) (if x #t y))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (list . l) l)

(define (length l)
  (if (null? l) 0
      (b+ 1 (length (cdr l)))))

; n-ary map not yet implemented
(define (map f l)
  (if (null? l) '()
      (cons (f (car l)) (map f (cdr l)))))

; n-ary for-each not yet implemented
(define (for-each f l)
  (if (null? l) '()
      (begin (f (car l)) (for-each f (cdr l)))))

(define (eof-object? x)
  (eq? x 'end-of-file))

;; everything beyond here is written by me (aka its probably wrong)

;; binary gcd
(define (bgcd a b)
        (if (negative? a) (set! a (* a -1)))
        (if (negative? b) (set! b (* b -1)))
        (if (zero? a)
                b
                (if (zero? b) 
                        a
                        (if (= a b)
                                a
                                (if (> a b)
                                        (bgcd (- a b) b)
                                        (bgcd a (- b a))
                                )
                        )
                )
        )
)

(define (b/ x y)
        (if (and (s48-integer? x) (s48-integer? y))
                (if (= y 0)
                        (type-error "cannot divide by zero" 1)
                        (if (= x 0)
                                0
                                (let ((g (gcd x y)))
                                        (if (= x y)
                                                1
                                                (if (= x 0)
                                                        0
                                                        (if (= g y)
                                                                (s48-/ x y)
                                                                (rational x y)
                                                        )
                                                )
                                        )
                                )
                        )
                )
                (if (or (rational? x) (rational? y))
                        (simplify (rational (* (numerator x) (denominator y)) (* (denominator x) (numerator y))))
                        (type-error "arguments of b/ are not integers" 1)
                )
        )
)

(define (b* x y)
        (if (and (s48-integer? x) (s48-integer? y))
                (s48-* x y)
                (if (and (rational? x) (rational? y))
                        (simplify (rational (s48-* (numerator x) (numerator y)) (s48-* (denominator x) (denominator y))))
                        (type-error "arguments of b* are not integers" 1)
                )
        )
)

(define (b+ x y)
        (if (s48-integer? x)
                (if (s48-integer? y)
	                (s48-+ x y)
	                (type-error "arguments of b+ are not integers" 0)
                )
                (type-error "arguments of b+ are not integers" 0)
        )
) 

(define s48-number? number?)

(define (rational x y) 
        (list 'rational x y)
)

(define (rational? arg)
        (if (s48-number? arg) 
                #t 
                (if (pair? arg)
                        (if (eq? 'rational (car arg)) 
                                #t
                                #f
                        )
                        #f
                )
        )
)

(define (number? arg) 
        (rational? arg)
)

(define (integer? arg)
        (if (s48-number? arg)
                #t
                #f
        )
)

(define (num x) 
        (cadr x)
)

(define (den x) 
        (caddr x)
)

(define (numerator x)
        (let ((x (simplify x)))
                (if (integer? x)
                        x
                        (num x)
                )
        )
)

(define (denominator x)
        (let ((x (simplify x)))
                (if (integer? x)
                        1
                        (den x)
                )
        )
)

(define (positive? x)
        (> (numerator x) 0)
)

(define (negative? x)
        (< (numerator x) 0)
)

(define (zero? x)
        (= (numerator x) 0)
)

;; nary gcd
(define (gcd . l)
        (if (= (length l) 0) 
                0
                (if (= (length l) 1)
                        (car l)
                        (if (= (length l) 2) 
                                (bgcd (car l) (car (cdr l))) 
                                (bgcd (car l) (apply gcd (cdr l)))
                        )
                )
        )
)

;; doesn't turn rationals into integers
(define (simplify x)
        (if (integer? x)
                x
                (if (rational? x)
                        (let (
                                (n (/ (num x) (gcd (num x) (den x))))
                                (d (/ (den x) (gcd (num x) (den x))))
                              )
                              (b/ n d)
                        )
                (type-error "arguments of simplify are not numbers" 1)       
                )
        )
)

(define (cm numx numy denx deny)
        (let(
                (nx (s48-* numx deny))
                (ny (s48-* numy denx))
                (dx (s48-* denx deny))
                (dy (s48-* deny denx))
        )
                (cons (rational nx dx) (cons (rational ny dy) '()))
        )
)

(define (b+ a b)
        (if (and (s48-integer? a) (s48-integer? b))
                (s48-+ a b)
                (if (or (rational? a) (rational? b))
                        (let ((commonMult (cm (numerator a) (numerator b) (denominator a) (denominator b))))
                                (simplify (rational (s48-+ (num (car commonMult)) (num (car (cdr commonMult)))) (den (car commonMult)))) 
                        )
                )
        )       
)

(define (b- a b)
        (if (and (s48-integer? a) (s48-integer? b))
                (s48-- a b)
                (if (or (rational? a) (rational? b))
                        (let ((commonMult (cm (numerator a) (numerator b) (denominator a) (denominator b))))
                                (simplify (rational (s48-- (num (car commonMult)) (num (car (cdr commonMult)))) (den (car commonMult)))) 
                        )
                )
        )       
)

(define (b= x y)
        (let ((x (simplify x)) (y (simplify  y)))
                (if (and (s48-integer? x) (s48-integer? y))
                        (s48-= x y) 
                        (if (or (s48-integer? x) (s48-integer? y))
                                #f
                                (if (and (rational? x) (rational? y))
                                        (r= x y)
                                        (type-error "arguments of b= are not numbers" #f)
                                )
                        )
                )
        )
)

(define (r= x y) 
        (and (b= (num x) (num y)) (b= (den x) (den y)))
)

(define (equal? a b)
        (cond 
                ((eqv? a b)
                        #t)
                ((and (pair? a) (pair? b))
                        (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
                (else #f)
        )
)

(define (w x)
        (begin
                (display "(")
                (wr x)
        )
)

(define (wr x)
        (if (null? x)
                (display ")")
                (if (list? x) 
                        (if (rational? x)
                                (begin
                                        (write (numerator x))
                                        (display "/")
                                        (write (denominator x))
                                )
                                (begin
                                        (if (and (pair? (car x)) (not (rational? (car x))))
                                                (display "(")
                                        )
                                        (wr (car x))
                                        (if (not (null? (cdr x)))
                                                (display " ")
                                        )
                                        (wr (cdr x))
                                )
                        )
                        (begin
                                (write x)
                        )
                                
                )
        )
)