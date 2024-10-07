#lang racket
(displayln "Hola, Mundo")


;DESPLIEGUE DE POLINOMIOS.

;función display-p
(define (display-p p)
  (define (term-to-string coef exp)
    (cond
      [(= coef 0) ""] ; Ignorar coeficientes 0
      [(= exp 0) (number->string coef)] ; Término constante (sin 'x')
      [(= exp 1) (string-append (if (= coef 1) "" (number->string coef)) "x")] ; Término con exponente 1
      [else (string-append (if (= coef 1) "" (number->string coef)) "x^" (number->string exp))])) ; Otros exponentes
  
  (define (polynomial-to-string p exp)
    (if (null? p)
        ""
        (let ([term (term-to-string (car p) exp)]
              [rest (polynomial-to-string (cdr p) (+ exp 1))])
          (if (string=? term "")
              rest
              (if (string=? rest "")
                  term
                  (string-append term " + " rest)))))) ; Concatenar términos con '+'

  (display (polynomial-to-string p 0)))

(display-p '(1 0 0 3))

;SUMA DE POLINOMIOS

; Función para sumar dos polinomios
(define (sumar-2-polinomios p1 p2)
  (define len-diff (- (length p1) (length p2)))
  (if (> len-diff 0)
      (set! p2 (append (make-list len-diff 0) p2))
      (set! p1 (append (make-list (- len-diff) 0) p1)))
  (map + p1 p2))

; Función para eliminar los ceros finales de un polinomio
(define (simplificar-polinomio poly)
  (let loop ([poly poly])
    (if (and (not (empty? poly)) (= (last poly) 0))
        (loop (take poly (- (length poly) 1)))
        poly)))

; Función para sumar una lista arbitraria de polinomios
(define (+p . polys)
  (define result (first polys))
  (for ([p (rest polys)])
    (set! result (sumar-2-polinomios result p)))
  (simplificar-polinomio result))

; Ejemplo de uso
(+p '(2 3 1) '(1 0 4) '(3 2 1 1)) ; Suma de tres polinomios
(+p '(3 2 0 5) '(1 0 4 7));
(+p '(0 2 3 1) '(1 0 4 0) '(3 0 2 4))
(+p '(1 1 0 1 0) '(0 2 3 0 1) '(0 0 0 3 4) '(5 2 0 0 0 0))
(+p '(2 5 0 0 6) '(-2 -5 0 0 3))

; RESTA DE POLINOMIOS

; Función para restar dos polinomios
(define (restar-polinomios p1 p2)
  (define len-diff (- (length p1) (length p2)))
  (if (> len-diff 0)
      (set! p2 (append (make-list len-diff 0) p2))
      (set! p1 (append (make-list (- len-diff) 0) p1)))
  (map - p1 p2))

; Función para restar una lista arbitraria de polinomios (asociativa por la izquierda)
(define (restar-polynomials . polys)
  (define result (first polys))
  (for ([p (rest polys)])
    (set! result (restar-polinomios result p)))
  (simplificar-polinomio result))

; Función para eliminar los ceros finales de un polinomio (opcional, ya usada anteriormente)
(define (simplificar-polinomioS poly)
  (let loop ([poly poly])
    (if (and (not (empty? poly)) (= (last poly) 0))
        (loop (take poly (- (length poly) 1)))
        poly)))

; Ejemplos de uso:
(restar-polynomials '(3 2 0 5) '(1 0 4 7)) ; Ejemplo con dos polinomios
(restar-polynomials '(5 0 2) '(3 0 1) '(2 0 1)) ; Ejemplo con tres polinomios
(restar-polynomials '(5 0 1 0 3) '(2 0 2 0 4))

(newline)
(newline)
(newline)


;MULTIPLICACIÓN DE POLINOMIOS

;; Multiplica un término de un polinomio por todo el otro polinomio, con un desplazamiento correspondiente al exponente
(define (multiply-by-term term exp poly)
  (append (make-list exp 0)  ; Desplazar según el exponente
          (map (lambda (x) (* term x)) poly)))  ; Multiplicar cada coeficiente por el término

;; Suma dos polinomios, similar a lo que ya hicimos en la suma
(define (sum-coefficients p1 p2)
  (define (extend-with-zeros p len)
    (if (< (length p) len)
        (append p (make-list (- len (length p)) 0))  ; Añade ceros a la lista más pequeña
        p))
  
  (define max-length (max (length p1) (length p2)))
  (define p1-extended (extend-with-zeros p1 max-length))
  (define p2-extended (extend-with-zeros p2 max-length))
  
  (map + p1-extended p2-extended))  ; Suma de coeficientes

;; Multiplica dos polinomios
(define (mul-coefficients p1 p2)
  (define (mul-helper p1 exp)
    (if (null? p1)
        '()
        (sum-coefficients (multiply-by-term (car p1) exp p2) (mul-helper (cdr p1) (+ exp 1)))))
  (mul-helper p1 0))

;; Multiplica una cantidad arbitraria de polinomios
(define (*p . polynomials)
  (foldl mul-coefficients '(1) polynomials))  ; Acumula el producto de todos los polinomios


(*p '(1 2) '(1 1))  ; (1 + 2x) * (1 + x)
(*p '(1 0 2) '(2))  ; (x^2 + 2) * 2



