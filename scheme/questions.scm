(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (cond
    ((null? items) nil)
    (else
      (cons (proc (car items)) (map proc (cdr items)))))
  )

(define (cons-all first rests)
  (cond
    ((null? rests) nil)
    (else
      (append (list (cons first (car rests))) (cons-all first (cdr rests))))))

(define (zip pairs)
  (list (map car pairs) (map cadr pairs)
  ))

;; Problem 18
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN Question 18
  (define (combine index lst)
    (cond
      ((null? lst) nil)
      (else (cons (list index (car lst))
                  (combine (+ index 1) (cdr lst))))))
  (combine 0 s))
  ; END Question 18

;; Problem 19
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN Question 19
  (cond
    ((= total 0) '(nil))
    ((or (< total 0) (null? denoms)) nil)
    (else
      (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
            (list-change total (cdr denoms)))))
  )
  ; END Question 19

;; Problem 20
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((quoted? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
           (cons form (cons params (map analyze body)))
           ; END Question 20
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
           (map analyze (append (list (map analyze (cons 'lambda (list (car (zip values)) (analyze (car body)))))) (cadr (zip values))))
           ; END Question 20
           ))
        (else
         ; BEGIN Question 20
         (map analyze expr)
         ; END Question 20
         )))

;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  ; BEGIN Question 21
  'REPLACE-THIS-LINE
  )
  ; END Question 21
