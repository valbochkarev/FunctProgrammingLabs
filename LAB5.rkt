#lang racket
;Лабораторна робота 5
;ФІТ ПІ-41 Бочкарьов
;Варіант 2. Визначте узагальнений предикат рівності equ ?,
;який перевіряє два числа на рівність, і вставте його
;в пакет узагальненої арифметики з операціями +, -, *, /.
;Операція повинна працювати для натуральних чисел, раціональних і комплексних.
;;(define (install-math-package)
  ;; общее
  (define (type-tag datum) (if (pair? datum) (car datum) (error "Некорректно помеченные данные -- TYPE-TAG" datum)))
  (define (contents datum) (if (pair? datum) (cdr datum) (error "Некорректно помеченные данные -- CONTENTS" datum)))
  
  ;; комплесное
  (define (rectangular? z) (eq? (type-tag z) 'rectangular))
  (define (real-part z) (car (contents z)))
  (define (imag-part z) (car (cdr (contents z))))
  
  ;; натуральное
  (define (natural? z) (eq? (type-tag z) 'natural))
  (define (number z) (car (contents z)))

  ;; рациональное
  (define (rational? z) (eq? (type-tag z) 'rational))
  (define (correct-rat? z) (not (eq? (denum z) 0)))
  (define (num z) (car (contents z)))
  (define (denum z) (cdr (contents z)))

  ;; операции
  (define (add a b)
    (cond
      ((and (rectangular? a) (rectangular? b))
           (list 'rectangular (+ (real-part a) (real-part b)) (+ (imag-part a) (imag-part b))))
      ((and (natural? a) (natural? b))
           (list 'natural (+ (number a) (number b))))
      ((and (rational? a) (rational? b))
           (list 'rational (+ (* (num a) (denum b)) (* (num b) (denum b))) (* (denum a) (denum b))))
      (else "Добавьте определение")))
   (define (sub a b)
    (cond
      ((and (rectangular? a) (rectangular? b))
           (list 'rectangular (- (real-part a) (real-part b)) (- (imag-part a) (imag-part b))))
      ((and (natural? a) (natural? b))
           (list 'natural (- (number a) (number b))))
      ((and (rational? a) (rational? b))
           (list 'rational (- (* (num a) (denum b)) (* (num b) (denum b))) (* (denum a) (denum b))))
      (else "Добавьте определение")))
  (define (mul a b)
    (cond
      ((and (rectangular? a) (rectangular? b))
           (list 'rectangular (+ (* (real-part a) (real-part b)) (* (imag-part a) (imag-part b)))
                          (+ (* (real-part a) (imag-part b)) (* (imag-part a) (real-part b)))))
      ((and (natural? a) (natural? b))
           (list 'natural (* (number a) (number b))))
      ((and (rational? a) (rational? b))
           (list 'rational (* (num a) (num b)) (* (denum a) (denum b))))
      (else "Добавьте определение")))
  (define (div a b)
    (cond
      ((and (rectangular? a) (rectangular? b))
           (list 'rectangular
                 (if (and (= (real-part b) (imag-part b) 0))
                  '(ERROR: DIV BY ZERO!)
                 (list (+ (if (= (real-part b) 0) 0 (/ (real-part a) (real-part b)))
                          (if (= (imag-part b) 0) 0 (/ (imag-part a) (imag-part b))))
                       (- (if (= (real-part b) 0) 0 (/ (imag-part a) (real-part b)))
                          (if (= (imag-part b) 0) 0 (/ (real-part a) (imag-part b))))))))
      ((and (natural? a) (natural? b))
           (list 'natural (if (= (number b) 0)
                 '(ERROR: DIV BY ZERO!)
                 (quotient (number a) (number b)))))
      ((and (rational? a) (rational? b))
           (list 'rational (if (or (= (num b) 0))
                               '(ERROR: DIV BY ZERO!)
                               ((* (num a) (denum b)) (* (denum a) (num b))))))
      (else "Добавьте определение")))
  (define (equ? a b)
    (cond
      ((and (rectangular? a) (rectangular? b))
           (and (eq? (real-part a) (real-part b)) (eq? (imag-part a) (imag-part b))))
      ((and (natural? a) (natural? b))
           (eq? (number a) (number b)))
      ((and (rational? a) (rational? b))
           (eq? (* (num a) (denum b)) (* (denum a) (num b))))
      (else "Добавьте определение")))
    
  ;;'done)
(equ? '(rectangular 2 1) '(rectangular 2 1))
(equ? '(rectangular 2 1) '(rectangular 2 2))
(equ? '(rectangular 2 1) '(rectangular 3 1))
(div (mul (add '(rectangular 2 1) '(rectangular 3 -7)) (sub '(rectangular 3 3) '(rectangular 3 2))) '(rectangular 2 0))
(div '(natural 1) '(natural 5))
