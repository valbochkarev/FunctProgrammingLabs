#lang racket
;Лабораторна робота 6
;ФІТ ПІ-41 Бочкарьов
;Варіант 2. Розробити програму, яка множить та ділить раціональні функції,
;які подаються дробами, чисельник та знаменник яких є многочленами, наприклад: (х+1)/(x^3+1)
(define (mulexp exp1 exp2)
  (if (and (pair? exp1) (pair? exp2))
      (list (mulpoly    (car exp1)  (car exp2))
            '/ (mulpoly (cadr exp1) (cadr exp2)))
      (error "Некорректные данные mulexp")))
(define (divexp exp1 exp2)
  (if (and (pair? exp1) (pair? exp2))
      (list (mulpoly    (car exp1)  (cadr exp2))
            '/ (mulpoly (cadr exp1) (car exp2)))
      (error "Некорректные данные divexp")))
(define (mulpoly p1 p2) (mulpoly2 p2 p1 p2))
(define (mulpoly2 p2 ep1 ep2)
  (cond
    ((eq? ep1 '()) '())
    ((eq? ep2 '()) (mulpoly2 p2 (next ep1) p2))
    (else (append (list(list (+ (ex ep1) (ex ep2)) (* (val ep1) (val ep2)))) (mulpoly2 p2 ep1 (next ep2))))
    ))

;(define (makepoly poly) ())
(define (present poly) (if (null? poly) (error "Некорректные данные present") (car poly)))
(define (next poly) (if (null? (cdr poly)) '() (cdr poly)))
(define (ex elem) (if (pair? elem)  (caar elem) (error "Некорректные данные ex")))
(define (val elem) (if (pair? elem) (cadar elem) (error "Некорректные данные val")))

  (define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(divexp '(((1 1) (0 1)) ((3 1) (0 1))) '(((1 1) (0 2)) ((2 1) (0 5))))

;(div ((x+1) /(x^3+1)) / ((x+2) /(x^2+5)))
;(divexp '(((1 1) (0 1)) ((3 1) (0 1))) '(((1 1) (0 3)) ((2 1) (0 5))))
