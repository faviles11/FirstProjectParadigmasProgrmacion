#lang racket

; ---------------------------------------------------------------------
; 
; (c) 2024 
; EIF400 Paradigmas de Programación 
; 2do ciclo 2024 
; Proyecto #1 
;
; Jeffry Barquero Torres 118150438
; Fabian Aviles Chinchilla 118230661
;
;
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

(define (*p p1 p2)
  (define (add-to-result result term pos)
    (let ((padded (append (make-list pos 0) term)))
      (+p result padded)))
  (let loop ((p1 p1) (pos 0) (result '(0)))
    (if (null? p1)
        result
        (loop (cdr p1) (+ pos 1) (add-to-result result (map (lambda (x) (* x (car p1))) p2) pos)))))

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

(define (drv-p . polys)
  (map (lambda (p)
         (let loop ((p (cdr p)) (exp 1) (result '()))
           (if (null? p)
               (reverse result)
               (loop (cdr p) (+ exp 1) (cons (* (car p) exp) result)))))
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
  (expt x (/ 1 3)))

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
               (root2 (/ (- (- b) (sqrt discriminant)) (* 2 a)))
               (factors (list (list 1 (- root1)) (list 1 (- root2))))  ; Factores lineales
               )
          factors)))

  ;; Función auxiliar para calcular las raíces de un polinomio cúbico
  (define (cubic-roots a b c d)
    ;; Utiliza el método de Cardano para encontrar raíces
    (let* ((delta0 (- (* b b) (* 3 a c)))
           (delta1 (- (* 2 (expt b 3)) (* 9 a b c) (* 27 (expt a 2 d))))
           (C (cbrt (/ (+ delta1 (sqrt (- (expt delta1 2) (* 4 (expt delta0 3))))) 2)))
           (u1 (/ (- b) (* 3 a)))
           (u2 (/ (- (sqrt 3) C) (* 3 a)))
           (u3 (/ (+ delta0 C) (* 3 a))))
      ;; Calcula las raíces
      (list u1 u2 u3)))

  ;; Verifica el grado del polinomio y aplica la factorización correspondiente
  (let ((deg (degree p)))
    (cond
      ((= deg 2)  ; Polinomio cuadrático
       (let ((a (car p))
             (b (cadr p))
             (c (caddr p)))
         (let ((roots (quadratic-roots a b c)))
           (if (null? roots)
               (list p)  ; No se puede factorizar
               roots))))  ; Regresa los factores
      ((= deg 3)  ; Polinomio cúbico
       (let ((a (car p))
             (b (cadr p))
             (c (caddr p))
             (d (cadddr p)))
         (let ((roots (cubic-roots a b c d)))
           (if (null? roots)
               (list p)  ; No se puede factorizar
               roots))))  ; Regresa los factores
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
(display-p (+p '(1 2 3) '(3 2 1))) ; Output esperado: 4 + 4x + 4x^2
(display-p (+p '(0 1 0 1) '(1 0 1))) ; Output esperado: 1 + x + x^2 + x^3

