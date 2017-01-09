;Duc Nguyen
;CS 4337.001
;3/4/2016
;Programming Assignment 1
#lang racket
;lists used for testing
(define l1 '(1 2 3 4 5))
(define l2 '(a b c d e))
(define l3 '(一 二 三 四 五 六))
(define l4 '(A (B (C (D E))) F G (H I)))
;1.) My-reverse: reverses elements in a list
(define (my-reverse l)
  (define (sub l tmp) ;subroutine for my-reverse
  (if (null? l )
      tmp
      (sub (cdr l) (cons (car l) tmp)) ;recursive call to subroutine and swaps car with cdr
      )
    )
  (sub l '()) ;call of subroutine
)

;2.) my-map: outputs a mapping of the input set with the input function
(define (my-map foo l)
  (define (sub l tmp)
  (if (null? l )
      tmp
      (sub (cdr l) (append tmp (list(foo (car l))))) ;adds mapped element to tmp
      )
    )
  (sub l '()) ;calls subroutine
)
;3.) function-3: inputs a function and returns function with input 3
(define (function-3 foo) (foo 3)) ;inputs 3 into foo

;4.) zipper: puts nth element of 2 different list into a list in order of position
(define (zipper l1 l2)
  (define (sub l1 l2 tmp)
  (if (or (null? l1) (null? l2))
      tmp
      (sub (cdr l1) (cdr l2) (append tmp (list (cons (car l1) (list (car l2)))))))) ;forms list of nth element and adds to tmp
  (sub l1 l2 '()) ;calls subroutine
)
;5.) segregate
(define (segregate l)
  (define (sub l tmp1 tmp2)
    (if (null? l)
        (cons tmp1 (list tmp2)) ;combines odd and even list
        (if (= 0 (modulo (car l) 2)) ;boolean expression to determine odd or even
            (sub (cdr l) (append tmp1 (list (car l))) tmp2) ;adds to even list
            (sub (cdr l) tmp1 (append tmp2 (list (car l))))))) ;adds to odd list
  (sub l '() '()) ;calls subroutine
)
;6.) is-member?
(define (is-member? x l)
  (if (null? l)
      #f
      (if (equal? x (car l)) #t (is-member? x (cdr l)))) ;recursive call going down the list to check each element
)
;7.) my-sorted?
(define (my-sorted? l)
  (define (any<? a b) ;general less than function regardless of data type
    (cond ((and (number? a) (number? b)) (< a b))
          ((and (string? a) (string? b)) (string<? a b))
          (else (string<? (format "~A" a) (format "~A" b)))))
  (if (= 1 (length l))
      #t
      (if (any<? (cadr l) (car l)) #f (my-sorted? (cdr l)))) ;goes down list, verifying list is sorted in ascending order
)
;8.) my-flatten
(define (my-flatten l)
( cond ((null? l) '())
       ((pair? l) (append (my-flatten (car l)) (my-flatten (cdr l)))) ;recursive call to my-flatten
       (else (list l))) ;call to subroutine
  )
  

;9.) threshold
(define (threshold l x)
  (define (sub l x tmp)
  (if (null? l)
      tmp
      (if (>= (car l) x) (sub (cdr l) x tmp) (sub (cdr l) x (append tmp (list (car l)))) ;adds to list numbers that are below threshold, discards number that are above
          )
      )
    )
  (sub l x '()) ;call to subroutine
)

;10.) my-list-ref
(define (my-list-ref l x)
  (if (null? l)
      "index out of bounds"
      (if (= x 1) (car l) (my-list-ref (cdr l) (- x 1)) ;checks position and returns element at x
          )
      )
  )