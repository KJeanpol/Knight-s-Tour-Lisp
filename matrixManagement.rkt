#lang racket
;--------------------------------------------MATRIXX MANAGEMENT---------------------------------------------------
(define (outOfBounds myList pos)
  (cond ((> 0 pos) (write 'menorque0) #t)
        ((< (- (length myList) 1) pos) (write 'mayor) #t)
        (else #f)))

(define (updateValueX myList pos newValue)
  (cond  ( (outOfBounds myList pos) ((write 'posoutofbounds)) )
         (else (updateValueXAux myList pos newValue '() 0))
         )
      )

(define (updateValueXAux myList pos newValue finalList cont)
  (cond ((empty? myList) finalList)
        ((= cont pos) (updateValueXAux (cdr myList) pos newValue (append finalList(list newValue)) (+ cont 1)))
        (else (updateValueXAux (cdr myList) pos newValue (append finalList (list (car myList))) (+ cont 1)))
        )
  )

(define (updateValueIJ matrix i j newValue)
  (updateValueIJAux matrix i j newValue '() 0)
  )

(define (updateValueIJAux matrix i j newValue matrixFinal cont)
    (cond ((empty? matrix) matrixFinal)
        ((= cont i) (updateValueIJAux (cdr matrix) i j newValue (append matrixFinal (list (updateValueX (car matrix) j newValue))) (+ cont 1)))
        (else (updateValueIJAux (cdr matrix) i j newValue (append matrixFinal  (list (car matrix))) (+ cont 1)))
        )
  )
;------------------------------------------------------------ GENERATE MATRIX NXN ----------------------------------------------------

(define (generateMatrixI n cont myList)
  (cond
    ((= cont n) myList)
    (else (generateMatrixI n (+ cont 1) (append myList '(())))
   )
  )
 )


(define (generateList n cont value myList)
  (cond
    ((= cont n) myList)
    (else (generateList n (+ cont 1) value (append myList (list value)))
   )
  )
 )

(define (generateMatrixIJ n cont myList)
  (cond
    ((= cont n) myList)
    (else (generateMatrixIJ n (+ cont 1) (append myList (list(generateList n 0 0 '()))))
   )
  )
 )



;------------------------------------------------------------------TEST----------------------------------------------------
;(updateValueX '(1 2 3 6) 1 '(9))
;(updateValueIJ '((1 2 3)(3 2 5)(7 8 9)) 1 0 9)
;(generateMatrixI 5 0 '())
;(generateList 5 0 8 '())
(generateMatrixIJ 5 0 '())