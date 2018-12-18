#lang racket
;Лабораторна робота 3
;ФІТ ПІ-41 Бочкарьов
;Варіант 2. Кінцевий ланцюговий дріб (finite continued fraction) з k елементі має вигляд:
;f=(N1/(D1+N2/(D2+...Nk/Dk)...)
;Визначити процедуру так, щоб обчислення давало значення k-елементного кінцевого ланцюгового дробу.
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (lab3v2lambda a b k)
  (define (flambda a b k l)
    (if (and (> k 0) (not (= b 0))) 
      ((λ (q r)
         (flambda b r (- k 1) (append l (list q))))
        (quotient a b)
        (remainder a b)
      )
      
      l
    )
  )
  (flambda a b k '())
)


(define (lab3v2let a b k)
  (define (flet a b k l)
    (if (and (> k 0) (not (= b 0)))
      (let ((q (quotient a b))
            (r (remainder a b)))
           (flet b r (- k 1) (append l (list q))))
      l
    )
  )
  (flet a b k '())
)
