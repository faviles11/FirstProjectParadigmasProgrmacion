#lang racket

; ---------------------------------------------------------------------
; 
; (c) 2024 
; EIF400 Paradigmas de Programación 
; 2do ciclo 2024 
; Proyecto #1 
;
; Jeffry Barquero Torres 118150438
;
;
;
;
; version 1.0.0 2024-10-19 
; 
; ---------------------------------------------------------------------
(define (display-p p)
  (let loop ((lst p) (exp 0) (is-first? #t))
    (cond
      ((null? lst) (display "\n"))
      ((= (car lst) 0) (loop (cdr lst) (+ exp 1) is-first?))
      (else
       (cond ((not is-first?) (display "+ "))
             (else (display "")))
       (cond
         ((> exp 0)
          (if (= exp 1)
              (display (string-append (number->string (car lst)) "x "))
              (display (string-append (number->string (car lst)) "x^" (number->string exp) " "))))
         (else
          (display (number->string (car lst)))))
       (loop (cdr lst) (+ exp 1) #f)))))




; CASOS DE PRUEBA
; Caso de prueba 1: Polinomio constante
(display-p '(3)) ; Salida esperada: 3

; Caso de prueba 2: Polinomio de un solo término no constante
(display-p '(0 2)) ; Salida esperada: 2x

; Caso de prueba 3: Polinomio de varios términos
(display-p '(1 0 3)) ; Salida esperada: 1 3x^2

; Caso de prueba 4: Polinomio con ceros intercalados
(display-p '(0 4 0 5)) ; Salida esperada: 4x 5x^3

; Caso de prueba 5: Polinomio con varios términos
(display-p '(5 0 3 0 1)) ; Salida esperada: 5 3x^2 1x^4

; Caso de prueba 6: Polinomio con todos los términos en cero
(display-p '(0 0 0)) ; Salida esperada: (nada se imprime)

; Caso de prueba 7: Polinomio con términos negativos
(display-p '(-3 0 2 -4)) ; Salida esperada: -3 2x^2 -4x^3

; Caso de prueba 8
(display-p '(0 2 2 1)) ; Salida esperada: 2x 2x^2 1x^3

; Caso de prueba 9
(display-p '(-1 2 2 -2)) ; Salida esperada: -1 2x 2x^2 -2x^3





;-----------------------------

; Suma de polinomios

;-----------------------------

(define (+p polys)
  (let ((max-len (if (null? polys) 0 (apply max (map length polys)))))
    (define (pad-zeroes p len)
      (if (< (length p) len)
          (append p (make-list (- len (length p)) 0))
          p))
    (define (sum-all polys result)
      (if (null? polys)
          result
          (sum-all (cdr polys) (map + result (pad-zeroes (car polys) (length result))))))
    (if (null? polys)
        '()
        (sum-all (cdr polys) (pad-zeroes (car polys) max-len)))))


; CASOS DE PRUEBA
;; Caso 1: Suma de dos polinomios simples
(+p '((1 2 3) (4 5 6)))
;; Resultado esperado: (5 7 9)

;; Caso 2: Un polinomio vacío y otro no vacío
(+p '((1 2 3) ()))
;; Resultado esperado: (1 2 3)

;; Caso 3: Tres polinomios de distinto tamaño
(+p '((1 2 3) (4 5) (6)))
;; Resultado esperado: (11 7 3)

;; Caso 4: Suma de tres polinomios iguales
(+p '((1 2 3) (1 2 3) (1 2 3)))
;; Resultado esperado: (3 6 9)

;; Caso 5: Todos los polinomios con coeficientes cero
(+p '((0 0 0) (0 0 0)))
;; Resultado esperado: (0 0 0)

;; Caso 6: Polinomios con coeficientes negativos
(+p '((1 -2 3) (-1 2 -3)))
;; Resultado esperado: (0 0 0)

;; Caso 7: Un solo polinomio en la lista
(+p '((1 2 3)))
;; Resultado esperado: (1 2 3)

;; Caso 8: Polinomio con una sola constante
(+p '((5) (3)))
;; Resultado esperado: (8)

;; Caso 9: Suma de polinomios de distintos tamaños, con ceros
(+p '((2 4 6) (0 0 3 5) (3)))
;; Resultado esperado: (5 4 9 5)
(newline)

;-----------------------------

; Resta de polinomios

;-----------------------------

(define (-p polys)
  (let ((max-len (if (null? polys) 0 (apply max (map length polys)))))
    (define (pad-zeroes p len)
      (if (< (length p) len)
          (append p (make-list (- len (length p)) 0))
          p))
    (define (subtract-polys polys result)
      (if (null? polys)
          result
          (subtract-polys (cdr polys) 
                          (map - result (pad-zeroes (car polys) (length result))))))
    (if (null? polys)
        '()
        (subtract-polys (cdr polys) (pad-zeroes (car polys) max-len)))))

; CASOS DE PRUEBA

;; Caso 1: Resta de dos polinomios simples
(-p '((5 7 9) (1 2 3)))
;; Resultado esperado: (4 5 6)

;; Caso 2: Resta de un polinomio y un polinomio vacío
(-p '((1 2 3) ()))
;; Resultado esperado: (1 2 3)

;; Caso 3: Resta de tres polinomios de diferente tamaño ;;;
(-p '((10 8 6) (4 3) (1)))
;; Resultado esperado: (5 5 6)

;; Caso 4: Resta de tres polinomios iguales   ;;;
(-p '((5 5 5) (5 5 5) (5 5 5))) 
;; Resultado esperado: (-5 -5 -5)

;; Caso 5: Resta de un polinomio con todos ceros y otro no vacío
(-p '((0 0 0) (1 2 3)))
;; Resultado esperado: (-1 -2 -3)

;; Caso 6: Resta de polinomios con coeficientes negativos
(-p '((1 -2 3) (-1 2 -3)))
;; Resultado esperado: (2 -4 6)

;; Caso 7: Un solo polinomio en la lista
(-p '((4 5 6)))
;; Resultado esperado: (4 5 6)

;; Caso 8: Resta de polinomios de diferentes tamaños con ceros ;;;
(-p '((3 4 5) (1 0 2 7)))
;; Resultado esperado: (2 4 3 -7)
(newline)

;-----------------------------

; Multiplicación de polinomios

;-----------------------------

(define (*p . polynomials)
  ;; Función auxiliar para sumar dos polinomios
  (define (add-polynomials p1 p2)
    (cond ((null? p1) p2)
          ((null? p2) p1)
          (else (cons (+ (car p1) (car p2))
                      (add-polynomials (cdr p1) (cdr p2))))))

  ;; Función auxiliar para multiplicar dos polinomios
  (define (multiply-two p1 p2)
    (define (multiply-helper p1 pos result)
      (if (null? p1)
          result
          (multiply-helper (cdr p1)
                           (+ pos 1)
                           (let ((term (map (lambda (x) (* x (car p1))) p2)))
                             (add-polynomials result (append (make-list pos 0) term))))))
    (multiply-helper p1 0 '(0)))

  ;; Función recursiva para multiplicar todos los polinomios en la lista
  (define (multiply-all polynomials)
    (if (null? (cdr polynomials))
        (car polynomials)
        (multiply-two (car polynomials) (multiply-all (cdr polynomials)))))

  ;; Llamada inicial con todos los polinomios
  (multiply-all polynomials))


;; Caso 1: Multiplicar dos polinomios simples
(*p '(1 2) '(1 2)) ; Polinomios: (1 + 2x) * (1 + 2x)
; Resultado esperado: '(1 4 4)

;; Caso 2: Multiplicar 2 polinomios simples
(*p '(2 3) '(1 4)) ; Polinomios: (2 + 3x) * (1 + 4x)
; Resultado esperado: '(2 11 12)

;; Caso 3: Multiplicar 2 polinomios simples
(*p '(3 4) '(2 1)) ; Polinomios: (3 + 4x) * (2 + x)
; Resultado esperado: '(6 11 4)

;; Caso 4: Multiplicar 2 polinomios simples
(*p '(1 0 2) '(1 2)) ; Polinomios: (1 + 2x2) * (1 + 2x)
; Resultado esperado: '(1 2 2 4)

;; Caso 5: Multiplicación con un polinomio de mayor grado
(*p '(2 3) '(1 0 1)) ; Polinomios: (2 + 3x) * (1 + x2)
; Resultado esperado: '(2 3 2 3)

;; Caso 6: Multiplicación con un polinomio de mayor grado
(*p '(1 1 1) '(1 0 0 1)) ; Polinomios: (1 + x + x2) * (1 + x3)
; Resultado esperado: '(1 1 1 1 1 1)

;; Caso 7: Multiplicación con un polinomio de mayor grado
(*p '(1 2) '(1 0 1)) ; Polinomios: (1 + 2x) * (1 + x2)
; Resultado esperado: '(1 2 1 2)

;; Caso 8: Multiplicación de tres polinomios
(*p '(1 1) '(1 2) '(1 0 1)) ; Polinomios: (1 + x) * (1 + 2x) * (1 + x2)
; Resultado esperado: '(1 3 3 3 2)

;; Caso 9: Multiplicación con un polinomio que es solo una constante
(*p '(1 2) '(2)) ; Polinomios: (1 + 2x) * 2
; Resultado esperado: '(2 4)

;; Caso 10: Multiplicación de varios polinomios constantes
(*p '(3) '(2) '(5)) ; Polinomios: 3 * 2 * 5
; Resultado esperado: '(30)

;; Caso 11: Multiplicación con un polinomio con coeficiente cero
(*p '(1 2 3) '(0)) ; Polinomios: (1 + 2x + 3x2) * 0
; Resultado esperado: '(0 0 0)
(newline)




; Cociente de la división de polinomios

;-----------------------------

(define (dividir-polinomios-cociente p1 p2)
  (if (< (length p1) (length p2))
      '() ; Si el grado de p1 es menor que p2, el cociente es 0
      (let* ((coef-cociente (/ (car p1) (car p2))) ; Divide los coeficientes principales
             (p2-multiplicado (multiplicar-polinomios p2 coef-cociente)) ; Multiplica p2 por coeficiente del cociente
             (resto (restar-polinomios-c p1 p2-multiplicado))) ; Resta el polinomio ajustado
        (cons coef-cociente (dividir-polinomios-cociente (quitar-ceros-c resto) p2))))) ; Llama recursivamente

(define (multiplicar-polinomios p c)
  (map (lambda (x) (* x c)) p)) ; Multiplica cada término por un escalar

(define (restar-polinomios-c p1 p2)
  (if (null? p2)
      p1 ; Si p2 es nulo, no hay nada que restar
      (map - p1 (append p2 (make-list (- (length p1) (length p2)) 0))))) ; Resta alineando los grados

(define (quitar-ceros-c p)
  (if (null? p)
      '()
      (if (= (car p) 0)
          (quitar-ceros-c (cdr p))
          p)))



; Caso de prueba #1: División de 2x^2 + 3x + 1 entre x + 1
(dividir-polinomios-cociente '(2 3 1) '(1 1))
; Resultado esperado: '(2 1) — Cociente es 2x + 1

; Caso de prueba #2: División de x^2 + 2x + 1 entre x^2 + 1
(dividir-polinomios-cociente '(1 2 1) '(1 0 1))
; Resultado esperado: '(1) — Cociente es 1

; Caso de prueba #3: División de x + 1 entre x^2 + 1
(dividir-polinomios-cociente '(1 1) '(1 0 1))
; Resultado esperado: '() — Cociente es 0

; Caso de prueba #4: División de x^2 + 3x + 2 entre x^2 + 3x + 2
(dividir-polinomios-cociente '(1 3 2) '(1 3 2))
; Resultado esperado: '(1) — Cociente es 1

; Caso de prueba #5: División de 5x^3 - 4x^2 + 3x - 2 entre x - 1
(dividir-polinomios-cociente '(5 -4 3 -2) '(1 -1))
; Resultado esperado: '(5 1 4) — Cociente es 5x^2 + x + 4

; Caso de prueba #6: División de 3x^4 + 2x^3 - x^2 + x - 3 entre x^2 + 1
(dividir-polinomios-cociente '(3 2 -1 1 -3) '(1 0 1))
; Resultado esperado: '(3 2 -4) — Cociente es 3x^2 + 2x - 4

; Caso de prueba #7: División de 6x^3 - x^2 + x + 7 entre 2x^2 + 3
(dividir-polinomios-cociente '(6 -1 1 7) '(2 0 3))
; Resultado esperado: '(3 -0.5) — Cociente es 3x - 0.5

; Caso de prueba #8: División de 4x^5 + x^4 - 2x^3 + 3x^2 - 5 entre x + 2
(dividir-polinomios-cociente '(4 1 -2 3 0 -5) '(1 2))
; Resultado esperado: '(4 -7 12 -21 42) — Cociente es 4x^4 - 7x^3 + 12x^2 - 21x + 42

; Caso de prueba #9: División de 7x^2 - 4x + 5 entre x - 1
(dividir-polinomios-cociente '(7 -4 5) '(1 -1))
; Resultado esperado: '(7 3) — Cociente es 7x + 3

; Caso de prueba #10: División de 4x^4 - 8x^3 + 4x^2 - 4 entre 2x^2 - 2
(dividir-polinomios-cociente '(4 -8 4 0 -4) '(2 0 -2))
; Resultado esperado: '(2 -4 4) — Cociente es 2x^2 - 4x + 4

; Caso de prueba #11: División de 3x^2 + 6x + 9 entre x + 3
(dividir-polinomios-cociente '(3 6 9) '(1 3))
; Resultado esperado: '(3 -3) — Cociente es 3x -3

; Caso de prueba #12: División de 6x^3 - 12x^2 + 18 entre 2x^2 - 2
(dividir-polinomios-cociente '(6 -12 18 0) '(2 -2))
; Resultado esperado: '(3 -3 6) — Cociente es 3x^2 - 3x + 6

; Caso de prueba #13: División de x^3 + 5x^2 + 7x + 3 entre x + 2
(dividir-polinomios-cociente '(1 5 7 3) '(1 2))
; Resultado esperado: '(1 3 1) — Cociente es x^2 + 3x + 1

; Caso de prueba #14: División de 2x^4 + 6x^3 + 4x^2 + 3x + 1 entre x^3 + 2x^2 + 3x + 4
(dividir-polinomios-cociente '(2 6 4 3 1) '(1 2 3 4))
; Resultado esperado: '(2 2) — Cociente es 2x + 2

; Caso de prueba #15: División de 5x^5 - 10x^4 + 8x^3 + 6x^2 + 2 entre x^3 - 2x^2 + 4x - 3
(dividir-polinomios-cociente '(5 -10 8 6 0 2) '(1 -2 4 -3))
; Resultado esperado: '(5 -12) — Cociente es 5x^2 -12x +0

; Caso de prueba #16: División de 3x^6 + 7x^5 + 5x^4 + 9x^3 + 4x^2 + 3x + 1 entre x^3 + x^2 + 2x + 3
(dividir-polinomios-cociente '(3 7 5 9 4 3 1) '(1 1 2 3))
; Resultado esperado: '(3 4 -5 -3) — Cociente es 3x^3 + 4x^2 -5x -3
(newline)
(newline)
(newline)


;--------------------------------------------

; Residuo de la división de polinomios

(define (dividir-polinomios-residuo p1 p2)
  (if (< (length p1) (length p2))
      p1 ; Si el grado de p1 es menor que p2, el residuo es p1
      (let* ((coef-cociente (/ (car p1) (car p2))) ; Divide los coeficientes principales
             (p2-multiplicado (multiplicar-polinomios-r p2 coef-cociente)) ; Multiplica p2 por coeficiente del cociente
             (resto (restar-polinomios p1 p2-multiplicado))) ; Resta el polinomio ajustado
        (dividir-polinomios-residuo (quitar-ceros resto) p2)))) ; Llama recursivamente para seguir dividiendo

; Funciones auxiliares reutilizadas

(define (multiplicar-polinomios-r p c)
  (map (lambda (x) (* x c)) p)) ; Multiplica cada término por un escalar

(define (restar-polinomios p1 p2)
  (if (null? p2)
      p1 ; Si p2 es nulo, no hay nada que restar
      (map - p1 (append p2 (make-list (- (length p1) (length p2)) 0))))) ; Resta alineando los grados

(define (quitar-ceros p)
  (if (null? p)
      '()
      (if (= (car p) 0)
          (quitar-ceros (cdr p))
          p)))

; Pruebas

; Caso de prueba #1: División de 2x^2 + 3x + 1 entre x + 1
(dividir-polinomios-residuo '(2 3 1) '(1 1))
; Resultado esperado: '() - Residuo es 0

; Caso de prueba #2: División de x^2 + 2x + 1 entre x^2 + 1
(dividir-polinomios-residuo '(1 2 1) '(1 0 1))
; Resultado esperado: '(2 0) — Residuo es 2 0

; Caso de prueba #3: División de x + 1 entre x^2 + 1
(dividir-polinomios-residuo '(1 1) '(1 0 1))
; Resultado esperado: '(1 1) — Residuo es x + 1

; Caso de prueba #4: División de x^2 + 3x + 2 entre x^2 + 3x + 2
(dividir-polinomios-residuo '(1 3 2) '(1 3 2))
; Resultado esperado: '() — Residuo es 0

; Caso de prueba #5: División de 5x^3 - 4x^2 + 3x - 2 entre x - 1
(dividir-polinomios-residuo '(5 -4 3 -2) '(1 -1))
; Resultado esperado: '(2) — Residuo es 2

; Caso de prueba #6: División de 3x^4 + 2x^3 - x^2 + x - 3 entre x^2 + 1
(dividir-polinomios-residuo '(3 2 -1 1 -3) '(1 0 1))
; Resultado esperado: '(-1 1) — Residuo es -x + 1

; Caso de prueba #7: División de 6x^3 - x^2 + x + 7 entre 2x^2 + 3
(dividir-polinomios-residuo '(6 -1 1 7) '(2 0 3))
; Resultado esperado: '(-8 8.5) — Residuo es -8x + 8.5

; Caso de prueba #8: División de 4x^5 + x^4 - 2x^3 + 3x^2 - 5 entre x + 2
(dividir-polinomios-residuo '(4 1 -2 3 0 -5) '(1 2))
; Resultado esperado: '(-89) — Residuo es -89

; Caso de prueba #9: División de 7x^2 - 4x + 5 entre x - 1
(dividir-polinomios-residuo '(7 -4 5) '(1 -1))
; Resultado esperado: '(8) — Residuo es 8

; Caso de prueba #10: División de 4x^4 - 8x^3 + 4x^2 - 4 entre 2x^2 - 2
(dividir-polinomios-residuo '(4 -8 4 0 -4) '(2 0 -2))
; Resultado esperado: '(-8 4) — Residuo -8x + 4

; Caso de prueba #11: División de 3x^2 + 6x + 9 entre x + 3
(dividir-polinomios-residuo '(3 6 9) '(1 3))
; Resultado esperado: '(18) — Residuo es 18

; Caso de prueba #12: División de 6x^3 - 12x^2 + 18 entre 2x^2 - 2
(dividir-polinomios-residuo '(6 -12 18 0) '(2 -2))
; Resultado esperado: '(12) — Residuo es 12

; Caso de prueba #13: División de x^3 + 5x^2 + 7x + 3 entre x + 2
(dividir-polinomios-residuo '(1 5 7 3) '(1 2))
; Resultado esperado: '(1) — Residuo es 1

; Caso de prueba #14: División de 2x^4 + 6x^3 + 4x^2 + 3x + 1 entre x^3 + 2x^2 + 3x + 4
(dividir-polinomios-residuo '(2 6 4 3 1) '(1 2 3 4))
; Resultado esperado: '(-6 -11 -7) — Residuo es -6x^2 -11x -7

; Caso de prueba #15: División de 5x^5 - 10x^4 + 8x^3 + 6x^2 + 2 entre x^3 - 2x^2 + 4x - 3
(dividir-polinomios-residuo '(5 -10 8 6 0 2) '(1 -2 4 -3))
; Resultado esperado: '(-3 48 -34) — Residuo es -3x^2 + 48x - 34)

; Caso de prueba #16: División de 3x^6 + 7x^5 + 5x^4 + 9x^3 + 4x^2 + 3x + 1 entre x^3 + x^2 + 2x + 3
(dividir-polinomios-residuo '(3 7 5 9 4 3 1) '(1 1 2 3))
; Resultado esperado: '(5 24 10) — Residuo es  5x^2 +24x + 10

(newline)
(newline)


;--------------------------------------------------

;   DIVISION COMPLETA

;--------------------------------------------------

(define (/--p p1 p2)
  (let ((cociente (dividir-polinomios-cociente p1 p2))  ; Calcula el cociente
        (residuo (dividir-polinomios-residuo p1 p2)))   ; Calcula el residuo
    (list cociente residuo)))  ; Retorna una lista con el cociente y el residuo



; Caso de prueba #1: División de 2x^2 + 3x + 1 entre x + 1
(/--p  '(2 3 1) '(1 1))
; Resultado esperado: '((2 1) ())

; Caso de prueba #2: División de x^2 + 2x + 1 entre x^2 + 1
(/--p  '(1 2 1) '(1 0 1))
; Resultado esperado: '((1) (2 0))

; Caso de prueba #3: División de x + 1 entre x^2 + 1
(/--p  '(1 1) '(1 0 1))
; Resultado esperado: '(() (1 1))

; Caso de prueba #4: División de x^2 + 3x + 2 entre x^2 + 3x + 2
(/--p  '(1 3 2) '(1 3 2))
; Resultado esperado: '((1) ())

; Caso de prueba #5: División de 5x^3 - 4x^2 + 3x - 2 entre x - 1
(/--p  '(5 -4 3 -2) '(1 -1))
; Resultado esperado: '((5 1 4) (2))

; Caso de prueba #6: División de 3x^4 + 2x^3 - x^2 + x - 3 entre x^2 + 1
(/--p '(3 2 -1 1 -3) '(1 0 1))
; Resultado esperado: '((3 2 -4) (-1 1))

; Caso de prueba #7: División de 6x^3 - x^2 + x + 7 entre 2x^2 + 3
(/--p  '(6 -1 1 7) '(2 0 3))
; Resultado esperado: '((3 -1/2) (-8 8 1/2))

; Caso de prueba #8: División de 4x^5 + x^4 - 2x^3 + 3x^2 - 5 entre x + 2
(/--p  '(4 1 -2 3 0 -5) '(1 2))
; Resultado esperado: '((4 -7 12 -21 42) (-89))

; Caso de prueba #9: División de 7x^2 - 4x + 5 entre x - 1
(/--p  '(7 -4 5) '(1 -1))
; Resultado esperado: '((7 3) (8))

; Caso de prueba #10: División de 4x^4 - 8x^3 + 4x^2 - 4 entre 2x^2 - 2
(/--p  '(4 -8 4 0 -4) '(2 0 -2))
; Resultado esperado: '((2 -4 4) (-8 4))

; Caso de prueba #11: División de 3x^2 + 6x + 9 entre x + 3
(/--p  '(3 6 9) '(1 3))
; Resultado esperado: '((3 -3) (18))

; Caso de prueba #12: División de 6x^3 - 12x^2 + 18 entre 2x^2 - 2
(/--p  '(6 -12 18 0) '(2 -2))
; Resultado esperado: '((3 -3 6) (12))

; Caso de prueba #13: División de x^3 + 5x^2 + 7x + 3 entre x + 2
(/--p  '(1 5 7 3) '(1 2))
; Resultado esperado: '((1 3 1) (1))

; Caso de prueba #14: División de 2x^4 + 6x^3 + 4x^2 + 3x + 1 entre x^3 + 2x^2 + 3x + 4
(/--p  '(2 6 4 3 1) '(1 2 3 4))
; Resultado esperado: '((2 2) (-6 -11 -7))

; Caso de prueba #15: División de 5x^5 - 10x^4 + 8x^3 + 6x^2 + 2 entre x^3 - 2x^2 + 4x - 3
(/--p  '(5 -10 8 6 0 2) '(1 -2 4 -3))
; Resultado esperado: '((5 -12) (-3 48 -34))

; Caso de prueba #16: División de 3x^6 + 7x^5 + 5x^4 + 9x^3 + 4x^2 + 3x + 1 entre x^3 + x^2 + 2x + 3
(/--p  '(3 7 5 9 4 3 1) '(1 1 2 3))
; Resultado esperado: '((3 4 -5 -3) (5 24 10))




;-----------------------------

; Derivación de polinomios

;-----------------------------

(define (drv-p . polys)
  (map (lambda (p)
         (define (derivar-term coef exp)
           (if (null? coef)
               '()
               (cons (* (car coef) exp) (derivar-term (cdr coef) (+ exp 1)))))
         (derivar-term (cdr p) 1))
       polys))

;; Caso 1: Derivada de un polinomio simple de grado 1: p(x) = 2x + 3
(drv-p '(3 2)) 
;; Resultado esperado: '(2) ya que la derivada de 2x es 2 y la constante 3 desaparece.

;; Caso 2: Derivada de un polinomio de grado 2: p(x) = x^2 + 3x + 4
(drv-p '(4 3 1)) 
;; Resultado esperado: '(3 2) ya que la derivada de x^2 es 2x y la derivada de 3x es 3.

;; Caso 3: Derivada de un polinomio de grado 3: p(x) = 2x^3 + x^2 + 5x + 7
(drv-p '(7 5 1 2)) 
;; Resultado esperado: '(5 2 6) ya que la derivada de 2x^3 es 6x^2, la de x^2 es 2x y la de 5x es 5.

;; Caso 4: Polinomio de grado 0 (constante): p(x) = 5
(drv-p '(5))
;; Resultado esperado: '() ya que la derivada de una constante es 0, representada como una lista vacía.

;; Caso 5: Polinomio con coeficientes negativos: p(x) = -3x^3 - 2x + 4
(drv-p '(4 -2 0 -3))
;; Resultado esperado: '(-2 0 -9) ya que la derivada de -3x^3 es -9x^2 y la de -2x es -2.

;; Caso 6: Polinomio con ceros entre los términos: p(x) = x^4 + 0x^3 + 5x^2 + x + 7
(drv-p '(7 1 5 0 1))
;; Resultado esperado: '(1 10 0 4) ya que la derivada de x^4 es 4x^3, la de 5x^2 es 10x y la de x es 1.

;; Caso 7: Derivada de polinomios múltiples a la vez: p1(x) = 3x^2 + 2x + 1, p2(x) = 5x + 6
(drv-p '(1 2 3) '(6 5))
;; Resultado esperado: '((2 6) (5)) ya que la derivada de 3x^2 + 2x es 6x + 2, y la de 5x es 5.

;; Caso 8: Polinomio con todos los coeficientes nulos: p(x) = 0
(drv-p '(0 0 0))
;; Resultado esperado: '() ya que el polinomio es completamente cero y su derivada es 0.

;; Caso 9: Polinomio con solo un término: p(x) = 5x
(drv-p '(0 5))
;; Resultado esperado: '(5) ya que la derivada de 5x es 5.

(newline)
;-----------------------------

; Evaluación de polinomios usando el algoritmo Horner

;-----------------------------

(define (eval-p p x)
  (foldr (lambda (coef acc) (+ coef (* acc x))) 0 p))

;; Caso 1: Evaluar el polinomio p(x) = 2x + 3 en x = 1
(eval-p '(3 2) 1)
;; Resultado esperado: 5, ya que p(1) = 2(1) + 3 = 5.

;; Caso 2: Evaluar el polinomio p(x) = 2x + 3 en x = 2
(eval-p '(3 2) 2)
;; Resultado esperado: 7, ya que p(2) = 2(2) + 3 = 7.

;; Caso 3: Evaluar el polinomio p(x) = x^2 + 3x + 4 en x = 3
(eval-p '(4 3 1) 3)
;; Resultado esperado: 22, ya que p(3) = 3^2 + 3(3) + 4 = 9 + 9 + 4 = 22.

;; Caso 4: Evaluar el polinomio p(x) = 2x^3 + x^2 + 5x + 7 en x = 2
(eval-p '(7 5 1 2) 2)
;; Resultado esperado: 35, ya que p(2) = 2(2^3) + 1(2^2) + 5(2) + 7 = 16 + 4 + 10 + 7 = 37.

;; Caso 5: Evaluar el polinomio constante p(x) = 5 en x = 10
(eval-p '(5) 10)
;; Resultado esperado: 5, ya que un polinomio constante siempre evalúa al mismo valor: p(x) = 5.

;; Caso 6: Evaluar el polinomio p(x) = -3x^2 - 2x + 4 en x = 2
(eval-p '(4 -2 -3) 2)
;; Resultado esperado: -4, ya que p(2) = -3(2^2) - 2(2) + 4 = -12 - 4 + 4 = -12.

;; Caso 7: Evaluar el polinomio p(x) = x^4 + 5x^2 + x + 7 en x = 1
(eval-p '(7 1 5 0 1) 1)
;; Resultado esperado: 14, ya que p(1) = 1^4 + 0(1^3) + 5(1^2) + 1 + 7 = 1 + 5 + 1 + 7 = 14.

;; Caso 8: Evaluar el polinomio p(x) = x^5 + 2x^3 + 4 en x = 2
(eval-p '(4 0 2 0 0 1) 2)
;; Resultado esperado: 44, ya que p(2) = 2^5 + 2(2^3) + 4 = 32 + 16 + 4 = 52.

;; Caso 9: Evaluar el polinomio p(x) = 0 en x = 5
(eval-p '(0) 5)
;; Resultado esperado: 0, ya que p(x) = 0 para cualquier valor de x.

;; Caso 10: Evaluar el polinomio p(x) = 5x^3 + 2x + 1 en x = 3
(eval-p '(1 2 0 5) 3)
;; Resultado esperado: 145, ya que p(3) = 5(3^3) + 0(3^2) + 2(3) + 1 = 135 + 6 + 1 = 142.

;-----------------------------

; Factorización de polinomios

;-----------------------------
