#lang racket
;Лабораторна робота 4
;ФІТ ПІ-41 Бочкарьов
;Вариант2. Напишіть процедуру, яка бере як аргумент дерево,
;представлене у вигляді списку, і повертає список,
;елементи якого - усі листя дерева, впорядковане зліва направо.
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (leaveslist tree)
  (cond ((null? tree) '())
    ((not (pair? tree)) (list tree))
    (else (append (leaveslist (car tree)) (leaveslist (cdr tree))))
  )
)

(define (RtoLleaveslist tree)
  (cond ((null? tree) '())
    ((not (pair? tree)) (list tree))
    (else (append (RtoLleaveslist (cdr tree)) (RtoLleaveslist (car tree))))
  )
)

(leaveslist (list 1 (list 3 4 5) 2))
(leaveslist (list 1 (list '()) 2))
(list 'до 1 (list (list (list '()) ) ) 3 4)
(leaveslist (list  'после 1 (list (list (list '()) ) 2) 3 4))
(list 'до 1 (list (list (list 1 2 25) ) '()) 3 4)
(leaveslist (list 'после 1 (list (list (list 1 2 25) ) '()) 3 4))
(RtoLleaveslist (list 'справа 'налево 1 (list (list (list 1 2 25) ) '()) 3 4))
(leaveslist '(1 2 (()(() 3)) 5))