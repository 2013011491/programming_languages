#lang racket
(provide (all-defined-out))

(define (check_bst x)
  (if (and (null? (cadr x)) (null? (caddr x)))
      #t
      (if (null? (cadr x))
          (if (< (car x) (caaddr x))
              (check_bst (caddr x))
              #f)
          (if (null? (caddr x))
              (check_bst (cadr x))
              (if (and (< (caadr x) (car x)) (< (car x) (caaddr x)))
                  (and (check_bst (caddr x)) (check_bst (cadr x)))
                  #f)))))

(define (apply f x)
  (if (null? x)
      '()
      (cons (f (car x)) (cons (apply f (cadr x)) (cons (apply f (caddr x)) '())))))

(define (equals x y)
  (equal? (sort (flatten x) <) (sort (flatten y) <)))
  