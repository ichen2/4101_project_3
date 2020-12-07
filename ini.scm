(define s48-number? number?)

(define (eqv? a b)
  (if (and (number? a) (number? b)) (b= a b)
      (eq? a b)))

(define (b> x y) (b< y x))
(define (b<= x y) (or (b= x y) (b< x y)))
(define (b>= x y) (or (b= x y) (b> x y)))

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

(define (w a)
        (begin
                (if (and (list? a) (not (rational? a)))
                        (display "(")
                )
                (w-recursive a)
        )
)

(define (w-recursive a)
        (if (null? a)
                (display ")")
                (if (list? a) 
                        (if (rational? a)
                                (begin
                                        (write (numerator a))
                                        (display "/")
                                        (write (denominator a))
                                )
                                (begin
                                        (if (and (pair? (car a)) (not (rational? (car a))))
                                                (display "(")
                                        )
                                        (w-recursive (car a))
                                        (if (not (null? (cdr a)))
                                                (display " ")
                                        )
                                        (w-recursive (cdr a))
                                )
                        )
                        (begin
                                (write a)
                        )
                                
                )
        )
)

(define (b+ a b)
        (if (and (s48-integer? a) (s48-integer? b))
                (s48-+ a b)
                (if (or (rational? a) (rational? b))
                        (let ((cm (common-mult (numerator a) (numerator b) (denominator a) (denominator b))))
                                (simplify (rational (s48-+ (num (car cm)) (num (car (cdr cm)))) (den (car cm)))) 
                        )
                )
        )       
)

(define (+ . l)
  (if (null? l) 0
      (b+ (car l) (apply + (cdr l)))))

(define (b- a b)
        (if (and (s48-integer? a) (s48-integer? b))
                (s48-- a b)
                (if (or (rational? a) (rational? b))
                        (let ((cm (common-mult (numerator a) (numerator b) (denominator a) (denominator b))))
                                (simplify (rational (s48-- (num (car cm)) (num (car (cdr cm)))) (den (car cm)))) 
                        )
                )
        )       
)

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

(define (- . l)
        (if (null? l) 
                0
                (if (= (length l) 1)
                        (car l)
                        (b+ (car l) (apply r- (cdr l)))
                )
        )
)

(define (b* a b)
        (if (and (s48-integer? a) (s48-integer? b))
                (s48-* a b)
                (if (and (rational? a) (rational? b))
                        (simplify (rational (s48-* (numerator a) (numerator b)) (s48-* (denominator a) (denominator b))))
                        (type-error "arguments of b* are not integers" 1)
                )
        )
)

(define (* . l)
  (if (null? l) 1
      (b* (car l) (apply * (cdr l)))))

(define (b/ a b)
        (if (and (s48-integer? a) (s48-integer? b))
                (if (= b 0)
                        (type-error "cannot divide by zero" 1)
                        (if (= a 0)
                                0
                                (let ((g (gcd a b)))
                                        (if (= a b)
                                                1
                                                (if (= a 0)
                                                        0
                                                        (if (= g b)
                                                                (s48-/ a b)
                                                                (rational a b)
                                                        )
                                                )
                                        )
                                )
                        )
                )
                (if (or (rational? a) (rational? b))
                        (simplify (rational (* (numerator a) (denominator b)) (* (denominator a) (numerator b))))
                        (type-error "arguments of b/ are not integers" 1)
                )
        )
)

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

(define (b= a b)
        (let ((a (simplify a)) (b (simplify  b)))
                (if (and (s48-integer? a) (s48-integer? b))
                        (s48-= a b) 
                        (if (or (s48-integer? a) (s48-integer? b))
                                #f
                                (if (and (rational? a) (rational? b))
                                        (recursive-= a b)
                                        (type-error "arguments are not numbers" #f)
                                )
                        )
                )
        )
)

(define (recursive-= a b) 
        (and (b= (num a) (num b)) (b= (den a) (den b)))
)

(define (= x y . l)
  (if (null? l) (b= x y)
      (and (b= x y) (apply = (cons y l)))))

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

(define (rational a b) 
        (list 'rational a b)
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

(define (num a) 
        (cadr a)
)

(define (numerator a)
        (let ((a (simplify a)))
                (if (integer? a)
                        a
                        (num a)
                )
        )
)

(define (den a) 
        (caddr a)
)

(define (denominator a)
        (let ((a (simplify a)))
                (if (integer? a)
                        1
                        (den a)
                )
        )
)

(define (positive? a)
        (> (numerator a) 0)
)

(define (negative? a)
        (< (numerator a) 0)
)

(define (zero? a)
        (= (numerator a) 0)
)

(define (simplify a)
        (if (integer? a)
                a
                (if (rational? a)
                        (let (
                                (n (/ (num a) (gcd (num a) (den a))))
                                (d (/ (den a) (gcd (num a) (den a))))
                              )
                              (b/ n d)
                        )
                (type-error "arguments of simplify are not numbers" 1)       
                )
        )
)

(define (common-mult numerator1 numerator2 denominator1 denominator2)
        (let(
                (n1 (s48-* numerator1 denominator2))
                (n2 (s48-* numerator2 denominator1))
                (d1 (s48-* denominator1 denominator2))
                (d2 (s48-* denominator1 denominator2))
        )
                (cons (rational n1 d1) (cons (rational n2 d2) '()))
        )
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