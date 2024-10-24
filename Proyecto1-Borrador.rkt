#lang racket

; ---------------------------------------------------------------------
; 
; (c) 2024 
; EIF400 Paradigmas de Programación 
; 2do ciclo 2024 
; Proyecto #1 
;
; Fabian Aviles Chinchilla 118230661
; Francisco Arias Sanabria 117460586
; Jeffry Barquero Torres 118150438
; Jennifer Mejias Salazar 504450002
;
; version 1.0.0 2024-10-19 
; 
; ---------------------------------------------------------------------

; Despliegue de polinomios
(define (display-p p)
  (let loop ((lst p) (exp 0) (first-term? #t))
    (cond ((null? lst) (display "\n"))
          ((= (car lst) 0) (loop (cdr lst) (+ exp 1) first-term?))
          (else
           (let* ((coef (car lst))
                  (coef-str (cond
                              ((= exp 0) (number->string coef)) ; Mostrar el coeficiente para términos constantes
                              ((= coef 1) "")                   ; Omitir "1" para términos con variable
                              ((= coef -1) "-")                 ; Mostrar solo "-" para coeficiente -1
                              (else (number->string coef))))
                  (term (cond ((= exp 0) coef-str)
                              ((= exp 1) (string-append coef-str "x"))
                              (else (string-append coef-str "x^" (number->string exp))))))
             (when (not first-term?)
               (display (if (> coef 0) " + " " ")))
             (display term)
             (loop (cdr lst) (+ exp 1) #f))))))

;-----------------------------

; Suma de polinomios

;-----------------------------

(define (+p . polys)
  (let* ((max-len (apply max (map length polys))))
    (apply map +
           (map (lambda (p)
                  (append p (make-list (- max-len (length p)) 0)))
                polys))))

;-----------------------------

; Resta de polinomios

;-----------------------------

(define (-p . polys)
  (foldl (lambda (p1 p2)
           (+p p1 (map - p2)))
         (car polys)
         (cdr polys)))

;-----------------------------

; Multiplicación de polinomios

;-----------------------------

(define (*p . polys)
  (define (add-to-result result term pos)
    (let ((padded (append (make-list pos 0) term)))
      (+p result padded)))
  (define (multiply-two p1 p2)
    (let loop ((p1 p1) (pos 0) (result '(0)))
      (if (null? p1)
          result
          (loop (cdr p1) (+ pos 1) (add-to-result result (map (lambda (x) (* x (car p1))) p2) pos)))))
  (if (null? polys)
      '(0)
      (foldl multiply-two '(1) polys)))  ; foldl aplica la multiplicación sucesiva

;-----------------------------

; Cociente de la división de polinomios

;-----------------------------

(define (qt-p p1 p2)
  ;; Función auxiliar para obtener el grado del polinomio
  (define (degree p)
    (- (length p) 1))

  ;; Función auxiliar para multiplicar un polinomio por un monomio (coeficiente y exponente)
  (define (mul-monomial p coef exp)
    (map (lambda (x) (* x coef)) (append (make-list exp 0) p)))

  ;; División recursiva
  (define (divide p1 p2 quotient)
    (let ((deg1 (degree p1))
          (deg2 (degree p2)))
      (if (< deg1 deg2)
          (reverse quotient)  ; Si el grado de p1 es menor que el de p2, retornamos el cociente
          (let* ((coef (/ (car p1) (car p2)))  ; Cociente de los coeficientes principales
                 (exp (- deg1 deg2))           ; Diferencia de grados
                 (monomial (mul-monomial p2 coef exp))  ; Multiplicamos p2 por el monomio
                 (remainder (+p p1 (map - monomial))))  ; Resta del polinomio
            (divide remainder p2 (cons coef quotient))))))

  ;; Inicia la división recursiva con un cociente vacío
  (divide p1 p2 '()))

;-----------------------------

; Residuo de la división de polinomios

;-----------------------------

(define (rem-p p1 p2)
  ;; Función auxiliar para obtener el grado del polinomio
  (define (degree p)
    (- (length p) 1))

  ;; Función auxiliar para multiplicar un polinomio por un monomio (coeficiente y exponente)
  (define (mul-monomial p coef exp)
    (map (lambda (x) (* x coef)) (append (make-list exp 0) p)))

  ;; División recursiva para obtener el residuo
  (define (divide p1 p2)
    (let ((deg1 (degree p1))
          (deg2 (degree p2)))
      (if (< deg1 deg2)
          p1  ; Si el grado de p1 es menor que el de p2, p1 es el residuo
          (let* ((coef (/ (car p1) (car p2)))  ; Cociente de los coeficientes principales
                 (exp (- deg1 deg2))           ; Diferencia de grados
                 (monomial (mul-monomial p2 coef exp))  ; Multiplicamos p2 por el monomio
                 (remainder (+p p1 (map - monomial))))  ; Resta del polinomio
            (divide remainder p2)))))


  ;; Inicia la división recursiva para obtener el residuo
  (divide p1 p2))

;-----------------------------

; División completa

;-----------------------------

(define (/-p p1 p2)
  (list (qt-p p1 p2) (rem-p p1 p2)))

;-----------------------------

; Derivación de polinomios

;-----------------------------
(define (drv-p polys)
  (map (lambda (p)
         (let loop ((p p) (exp (- (length p) 1)) (result '()))
           (if (null? p)
               (reverse result)
               (if (= exp 0)
                   (reverse result)
                   (loop (cdr p) (- exp 1) (cons (* (car p) exp) result))))))
       polys))

;-----------------------------

; Evaluación de polinomios usando el algoritmo Horner

;-----------------------------

(define (eval-p p x)
  (foldr (lambda (coef acc) (+ coef (* acc x))) 0 p))

;-----------------------------
; Factorización de polinomios
;-----------------------------

(define (cbrt x)
  (if (>= x 0)
      (expt x (/ 1 3.0))
      (- (expt (- x) (/ 1 3.0)))))

(define (fact-p p)
  ;; Función auxiliar para calcular el grado del polinomio
  (define (degree p)
    (- (length p) 1))

  ;; Función auxiliar para calcular las raíces de un polinomio cuadrático
  (define (quadratic-roots a b c)
    (define discriminant (- (* b b) (* 4 a c)))
    (if (< discriminant 0)
        '()  ; No hay raíces reales
        (let* ((root1 (/ (+ (- b) (sqrt discriminant)) (* 2 a)))
               (root2 (/ (- (- b) (sqrt discriminant)) (* 2 a))))
          (list (list a (- root1)) (list a (- root2))))))  ; Factores lineales

  ;; Función auxiliar para calcular las raíces de un polinomio cúbico
  (define (cubic-roots a b c d)
    (if (= a 0)
        (quadratic-roots b c d)  ; Si a es 0, trata como cuadrático
        (let* ((delta0 (- (* b b) (* 3 a c)))
               (delta1 (- (* 2 (expt b 3)) (* 9 a b c) (* 27 (expt a 2) d)))
               (discriminant (+ (expt delta1 2) (* -4 (expt delta0 3))))
               (C (if (= delta0 0) 0 (cbrt (/ (+ delta1 (sqrt discriminant)) 2.0)))))
          (if (= C 0)
              (quadratic-roots b c d)
              (let* ((root1 (/ (+ (- b) C) (* 3 a)))
                     (root2 (/ (+ (- b) (* C (cos (/ (* 2 pi) 3)))) (* 3 a)))
                     (root3 (/ (+ (- b) (* C (cos (/ (* 4 pi) 3)))) (* 3 a))))
                (list (list a (- root1)) (list a (- root2)) (list a (- root3))))))))

  ;; Función auxiliar para normalizar factores
  (define (normalize-factors factors)
    (map (lambda (factor)
           (let ((gcd-factor (gcd (first factor) (abs (second factor)))))  ;; Asegúrate de que gcd se defina
             (list (/ (first factor) gcd-factor) (/ (second factor) gcd-factor))))
         factors))

  ;; Verifica el grado del polinomio y aplica la factorización correspondiente
  (let ((deg (degree p)))
    (cond
      ((= deg 2)  ; Polinomio cuadrático
       (let ((a (car p))
             (b (cadr p))
             (c (caddr p)))
         (if (= a 0)
             (quadratic-roots b c 0)  ; Si a es 0, pasamos a (b, c, 0)
             (let ((roots (quadratic-roots a b c)))
               (if (null? roots)
                   (list p)  ; No se puede factorizar
                   (normalize-factors roots))))))  ; Regresa los factores normalizados
      ((= deg 3)  ; Polinomio cúbico
       (let ((a (car p))
             (b (cadr p))
             (c (caddr p))
             (d (cadddr p)))
         (let ((roots (cubic-roots a b c d)))
           (if (null? roots)
               (list p)  ; No se puede factorizar
               (normalize-factors roots)))))  ; Regresa los factores normalizados
      (else
       (list p)))))  ; Grados superiores no manejados

#|
(display-p '(0))
(display-p '(0 0 0 1))
(display-p '(0 0 0 2))
(display-p '(0 0 1))
(display-p '(0 0 1 1))
(display-p '(0 0 1 2))
(display-p '(0 0 2))
(display-p '(0 0 2 1))
(display-p '(0 0 2 2))
(display-p '(0 1))
(display-p '(0 1 0 1))
(display-p '(0 1 0 2))
(display-p '(0 1 1))
(display-p '(0 1 1 1))
(display-p '(0 1 1 2))
(display-p '(0 1 2))
(display-p '(0 1 2 1))
(display-p '(0 1 2 2))
(display-p '(0 2))
(display-p '(0 2 0 1))
(display-p '(0 2 0 2))
(display-p '(0 2 1))
(display-p '(0 2 1 1))
(display-p '(0 2 1 2))
(display-p '(0 2 2))
(display-p '(0 2 2 1))
(display-p '(0 2 2 2))
(display-p '(1))
(display-p '(1 0 0 1))
(display-p '(1 0 0 2))
(display-p '(1 0 1))
(display-p '(1 0 1 1))
(display-p '(1 0 1 2))
(display-p '(1 0 2))
(display-p '(1 0 2 1))
(display-p '(1 0 2 2))
(display-p '(1 1))
(display-p '(1 1 0 1))
(display-p '(1 1 0 2))
(display-p '(1 1 1))
(display-p '(1 1 1 1))
(display-p '(1 1 1 2))
(display-p '(1 1 2))
(display-p '(1 1 2 1))
(display-p '(1 1 2 2))
(display-p '(1 2))
(display-p '(1 2 0 1))
(display-p '(1 2 0 2))
(display-p '(1 2 1))
(display-p '(1 2 1 1))
(display-p '(1 2 1 2))
(display-p '(1 2 2))
(display-p '(1 2 2 1))
(display-p '(1 2 2 2))
(display-p '(2))
(display-p '(2 0 0 1))
(display-p '(2 0 0 2))
(display-p '(2 0 1))
(display-p '(2 0 1 1))
(display-p '(2 0 1 2))
(display-p '(2 0 2))
(display-p '(2 0 2 1))
(display-p '(2 0 2 2))
(display-p '(2 1))
(display-p '(2 1 0 1))
(display-p '(2 1 0 2))
(display-p '(2 1 1))
(display-p '(2 1 1 1))
(display-p '(2 1 1 2))
(display-p '(2 1 2))
(display-p '(2 1 2 1))
(display-p '(2 1 2 2))
(display-p '(2 2))
(display-p '(2 2 0 1))
(display-p '(2 2 0 2))
(display-p '(2 2 1))
(display-p '(2 2 1 1))
(display-p '(2 2 1 2))
(display-p '(2 2 2))
(display-p '(2 2 2 1))
(display-p '(2 2 2 2))
(display-p '(0 0 -2 -2))
(display-p '(0 0 -1 -1))
(display-p '(-1 -1 0 -1))
(display-p '(1 1 1 -1))
(display-p '(-1 2 -1 -1))
(display-p '(-1 1 1 -2))
(display-p '(-2 -1 -2 1))
(display-p '(-2 0 -1 -1))
(display-p '(1 0 1 -1))
(display-p '(2 1 -1 1))
(display-p '(-2 -1 0 2))
(display-p '(1 -2 -1 2))
(display-p '(2 -2 2 2))
(display-p '(-2 1 -2 2))
(display-p '(0 -2 0 -1))
(display-p '(-1 -2 -2))
(display-p '(-2 -2 2 -2))
(display-p '(1 2 2 2))
(display-p '(2 2 1 2))
(display-p '(2 1 0 2))
(display-p '(2 -1 -1 -1))
(display-p '(-1 1 0 1))
(display-p '(2 -2 0 2))
(display-p '(-2 -2 0 2))
(display-p '(-1 -2 -1 1))
(display-p '(0 -2 2 -2))
(display-p '(1 -1 1 2))
(display-p '(2 0 -1))
(display-p '(0 2 0 1))
(display-p '(0 2 1 2))
(display-p '(0 -1 0 1))
(display-p '(-2 2 -2 -1))
(display-p '(1 1 1 2))
(display-p '(2 1))
(display-p '(-1 0 -1 1))
(display-p '(-1 2 2 -2))
(display-p '(1 1 1 -1))
(display-p '(-1 1 0 2))
(display-p '(1 0 1 1))
(display-p '(2 -1 -2 2))
|#

; Pruebas para la suma de polinomios (+p)
(display "Pruebas para +p:\n")
(display "Resultado: ")
(display-p (+p '(1 2 3) '(3 2 1))) ; Output esperado: 4 + 4x + 4x^2
(display "Resultado: ")
(display-p (+p '(0 1 0 1) '(1 0 1))) ; Output esperado: 1 + x + x^2 + x^3
(newline)

; Prueba para la multiplicación de polinomios (*p)
(display "Prueba para *p:\n")
(display "Resultado: ")
(display-p (*p '(1 2 3) '(3 2 1))) ; Output esperado: 3 + 8x + 14x^2 + 8x^3 + 3x^4
(display "Resultado: ")
(display-p (*p '(1 0 1) '(1 0 1))) ; Output esperado: 1 + 2x^2 + x^4
(display "Resultado: ")
(display-p (*p '(2 1) '(5 3))) ; Output esperado: 10 + 11x + 3x^2
(display "Resultado: ")
(display-p (*p '(1 2) '(3 4) '(2 0 1))) ; Output esperado: 6 + 20x + 19x^2 + 10x^3 + 8x^4
(newline)

; Pruebas para la derivación de polinomios (drv-p)
(display "Pruebas para drv-p:\n")
(display "Resultado: ")
(display (drv-p '((1 0 0 4)))) ; Output esperado: '((3 0 0))
(newline)
(display "Resultado: ")
(display (drv-p '((3 4 0 5)))) ; Output esperado: '((9 8 0))
(newline)
(display "Resultado: ")
(display (drv-p '((7)))) ; Output esperado: '()
(newline)
(display "Resultado: ")
(display (drv-p '((4 3 2 1) (2 3 4)))) ; Output esperado: '((12 6 2) (4 3))
(newline)
(display "Resultado: ")
(display (drv-p '((6 5 4 3 2)))) ; Output esperado: '((24 15 8 3))
(newline)
(newline)

; Pruebas para la evaluación de polinomios (eval-p)
(display "Pruebas para eval-p:\n")
(display "Resultado: ")
(display (eval-p '(1 3 2) 3)) ; Output esperado: 28
(newline)
(display "Resultado: ")
(display (eval-p '(5 3) 2)) ; Output esperado: 11
(newline)
(display "Resultado: ")
(display (eval-p '(5 1 2 3) -2)) ; Output esperado: -13
(newline)
(newline)

;-----------------------------
; Pruebas para la factorización de polinomios (fact-p)
;-----------------------------
(display "Pruebas para fact-p:\n")

(display (fact-p '(1 -5 6))) ; Output esperado: ((1 2) (1 3))
(newline)

(display (fact-p '(1 -6 11 -6))) ; Output esperado: ((1 1) (1 2) (1 3))
(newline)

(display (fact-p '(1 0 0 -16))) ; Output esperado: ((1 4) (1 -4) (1 0))
(newline)




