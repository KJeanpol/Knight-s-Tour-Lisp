#lang racket
;--------------------------------------------------------DEFINICION DE VARIABLES PARA GUI---------------------------------------------------------
(require (lib "graphics.ss" "graphics")); se importa la libreria graphics
(open-graphics) ; se abre el modo gráfico
(define z (open-viewport "Ajedrez" 660 660)); se declara una variable z que guarda una ventana de 660x660 con nombre Ajedrez
(define p (open-pixmap "Ajedreez" 660 660)) ; se declara una variable p que guarda una ventana oculta de 660X660 
(define negro "white") ; se define una variable negro que contendra el color negro
(define myMatrix null)
(define (setMatrix name)
(set! myMatrix name))(define gris (make-rgb 0.40 0.40 0.40)) ; se define una variable gris que contendra el color gris para el tablero
(define u 0) ; variable utilizada para dibujar el tablero intercalado 
(define h 0) ; posicion inicial del tablero eje x
(define v 0) ; posicion inicial del tablero eje y
(define rojo (make-rgb 1 0 0))
(define prueba null)
(define sizeScreen 640)
(define (setprueba n) ; Se usa para variar el tamaño de la cuadricula segun el tamaño ingresado
  (set! prueba (/ sizeScreen n)))
;--------------------------------------------------DIBUJA EL TABLERO----------------------------------------------

(define (game n)
  (setprueba n) ;iguala la variable n, al numero de la matriz 
  (for ([h (in-range 10 660 prueba)]) ;ciclo for que se encarga de crear las lineas verticales de la cuadricula
    ((draw-line p) (make-posn h 10) (make-posn h 650) "black")
    )
  (for ([v (in-range 10 660 prueba)])
    ((draw-line p) (make-posn 10 v) (make-posn 650 v) "black") ;ciclo for que se encarga de crear las lineas horizontales de la cuadricula
    )
  )
  
;---------------------------DIBUJA UN CUADRO ROJO QUE SEÑALA LOS MOVIMIENTOS DEL CABALLO EN EL TABLERO----------------------------------------
(define (moveKnight i j color)
  ((draw-solid-rectangle p) (make-posn  (+ (* prueba i) (/ prueba 6))  (+ (* prueba j) (/ prueba 6)) ) (- prueba 5) (- prueba 5) rojo) ;Dibuja un rectangulo en la posicion i j dada
  (copy-viewport p z)      ;Se utiliza para dibujar sobre la vista principal de la ventana
  )
  
;-----------------------------------------SE ENCARGA DE DIBUJAR LA RUTA DEL CABALLO EN EL GUI------------------------------
(define (PDC-Paint n matrixSol)  ; Llama a game para iniciar la ventana.
  (game n)
  (PDC-PaintAux matrixSol n 1)
  )
(define (PDC-PaintAux matrixSol n m)
  (cond
    (( > m (* n n)) (display "Finalizo con exito")) ;Si llega al final de la solucion, imprime en consola un mensaje de Exito de ejecución
    (else (sleep 1)(moveKnight (first(findStartB matrixSol 0 0 #f m)) (second (findStartB matrixSol 0 0 #f m)) rojo) (PDC-PaintAux matrixSol n (+ m 1)) ) ; Busca la posicion i j siguiente en la ruta y lo dibuja en la ventana.
    )
  )

;--------------------------------------- VERIFICA SI ESTA UN ELEMENTO DENTRO DE UNA LISTAS--------------------------
(define (isStartB myList has num)
  (cond
    ((empty? myList) has)		;Retorna false pues no encontro el elemento dentro de la lista
    (else (cond
            ((= (car myList) num) #t) ;Si esta el elemento dentro de la lista, retorna true de inmediato
            (else(isStartB (cdr myList) has num ; Itera sobre la lista llamando recursivamente a isStarb pero con el resto de la lista
            )
        )
    )
  )
;--------------------------------------- RETORNA LA POSICION DENTRO DE UNA LISTA, DE CIERTO ELEMENTO CONSULTADO--------------------------
(define (findIB myList n num) ;n es el contador de la lista, el cual tomara el valor de la posicion del elemento a buscar, num es el numero a buscar la posicion
  (cond
    ((empty? myList) n)
    (else (cond
            ((= (car myList) num) n) ;si el primer elemento de la lista es igual al num, retorna el valor de n en ese momento el cual será su posicion.
            (else (findIB (cdr myList) (+ n 1) num)) ;si no lo ha encontrado, llama a findIB pero con n+1, para así controlar el valor de la posicion
            ))
    )
  )
;--------------------------------------- RETORNA LA POSICION I J DENTRO DE UNA MATRIZ, DE CIERTO ELEMENTO CONSULTADO--------------------------
(define (findStartB myMatrix i j has num) ; como la matriz es basicamente, una lista de listas, llamara a findIB por cada elemento de myMatrix 
  (cond
    ((isStartB (car myMatrix) has num) (append (list i) (append '() (list (findIB (car myMatrix) 0 num)))))
    (else (findStartB (cdr myMatrix) (+ i 1) j has num))
    )
  )



;--------------------------------------------MATRIXX MANAGEMENT---------------------------------------------------
(define (outOfBounds myList pos)  ;verifica si la posicion consultada esta afuera de la lista
  (cond ((> 0 pos) (write 'menorque0) #t)
        ((< (- (length myList) 1) pos) (write 'mayor) #t)
        (else #f)))

(define (updateValueX myList pos newValue)	;actualiza un valor en la posicion dada, dentro de una lista, deja los demas valores en las otras posiciones igual
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

(define (updateValueIJAux matrix i j newValue matrixFinal cont) ; Actualiza un valor dentro de la matriz dado una posicion i j, llama a updateValueX dentro de ella, pues se manejaran listas por cada elemento del parametro matrix
    (cond ((empty? matrix) matrixFinal)
        ((= cont i) (updateValueIJAux (cdr matrix) i j newValue (append matrixFinal (list (updateValueX (car matrix) j newValue))) (+ cont 1)))
        (else (updateValueIJAux (cdr matrix) i j newValue (append matrixFinal  (list (car matrix))) (+ cont 1)))
        )
  )

(define (getIValue myList pos)
  (cond
    ((= pos 0) (car myList))
    (else(getIValue (cdr myList) (- pos 1)))
    )
  )

(define (getIJValue matrix i j)
  (getIValue (getIValue matrix i) j)
  )

;------------------------------------------------------------ GENERATE MATRIX NXN ----------------------------------------------------

(define (generateMatrixI n cont myList) ; Genera una matriz del tamaño nxn, con n el parametro que indica de cuanto sera la matriz cuadrada
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

(define (printList myList) ; Imprime en consola los valores de una lista dada.
  (cond
    ((empty? myList) (display "\n"))
    (else (write (car myList)) (display "  |  ") (printList (cdr myList)) ;divide cada valor de la lista mediante el simbolo |
    )
  )
  )

(define (printMatrix matrix N) ;Imprime en consola los valores de una matriz dada
  (cond
    ((empty? matrix)(display "\n"))
    (else (printList (car matrix)) (printMatrix (cdr matrix) (- N 1)) ; por cada elemento de la matriz, llama a printList para ir impimiendo cada fila
    )))


(define (isStart myList has) ; Verifica si es el inicio de la ruta, es decir si el valor es 1 dentro de la solucion de la ruta del caballo
  (cond
    ((empty? myList) has) ;Retorna false si no es el inicio
    (else (cond
            ((= (car myList) 1) #t) ;Retorna true si el primer elemento de la lista es igual a 1, valor inicial de la ruta.
            (else(isStart (cdr myList) has))
            )
        )
    )
  )

(define (findI myList n) ;Busca la posicion del 1 dentro de una lista
  (cond
    ((empty? myList) n)
    (else (cond
            ((= (car myList) 1) n)
            (else (findI (cdr myList) (+ n 1))) ; aumenta en 1 a n, para asi devolver su valor al final, el cual representa la posicion del inicio dentro de la lista
            ))
    )
  )

(define (findStart myMatrix i j has) ;Retorna una lista con la posicion i j del 1 dentro de la matriz.
  (cond
    ((isStart (car myMatrix) has) (append (list i) (append '() (list (findI (car myMatrix) 0)))))
    (else (findStart (cdr myMatrix) (+ i 1) j has))
    )
  )

(define (completeList myList myListAux n) ;Funcion utilizada para corregir las matrices, las cuales al final de la ejecución devuelven un valor en cero, esta funcion la completa con el valor correspondiente
  (cond
    ((empty? myList) myListAux)
    (else (cond
            ((= (car myList) 0) (completeList (cdr myList) (append myListAux (list(* n n))) n)) ;si encuentra un cero dentro de la lista, lo actualiza a un nxn, con n el tamaño de matriz cuadrada.
            (else (completeList (cdr myList) (append myListAux (list (car myList))) n)
            ))
    )
  ))

(define (completeMatrix myMatrix matrixAux n) ;Funcion que itera sobre ella, para compeltar la posicion con el valor 0, con el correspondiente, llamando a completeList
  (cond
    ((empty? myMatrix) matrixAux)
    (else (completeMatrix (cdr myMatrix) (append matrixAux (list (completeList (car myMatrix) '() n))) n)
    )
  ))

;------------------------------------------------------------------PDC-SOL----------------------------------------------------

(define(PDC-Sol n lista) ;Invoca la aux para tener control del los movimientos
  (setMatrix (generateMatrixIJ n 0 '()))  ; genera la matriz con indices en 0
  (aux-Sol n (* n n) myMatrix 1 (first lista) (second lista) (- n 1) '(-1 -2 -2 -1 1 2 2 1) '(-2 -1 1 2 2 1 -1 -2) 0 0))

(define (aux-Sol n ncuad matrix mov x y limit ejex ejey p sol);auxiliar
  

  (cond ((equal? mov 1) (aux-Sol n ncuad (setMatrix (updateValueIJ myMatrix x y mov)) (+ mov 1) x y limit ejex ejey p sol )) ;verifica el primer movimiento y lo anota en la matriz y aumenta el mov en 1
        ((equal? sol 1)(printMatrix (completeMatrix myMatrix '() n) n) (display (completeMatrix myMatrix '()n)) (exit)); verifica si existe solucion, esto se da cuando sol es igual a 1
        ((< p 8)    ; verifica si se recorrio todo el ejex y el ejey
                (cond ((and(and(and(>= (+ x (getIValue ejex p)) 0) (<=(+ x (getIValue ejex p)) limit)) (and(>= (+ y (getIValue ejey p)) 0) (<=(+ y (getIValue ejey p)) limit))) (equal? (getIJValue myMatrix (+ x (getIValue ejex p)) (+ y (getIValue ejey p)) ) 0)) ; verifica si la nueva x y, son moviminetos validos, si puede moverse ahi solo si en ese indice es 0
                (cond ((< mov ncuad) (aux-Sol n ncuad (setMatrix(updateValueIJ myMatrix (+ x (getIValue ejex p)) (+ y (getIValue ejey p)) mov)) (+ mov 1) (+ x (getIValue ejex p)) (+ y (getIValue ejey p)) limit ejex ejey 0 0 ) ; verifica si se han abarcado todos los movimientos ncuad, si no entonces actualiza el nuevo x y, y devuelve el mov aumentado en 1
                                
                                     (setMatrix(updateValueIJ myMatrix (+ x (getIValue ejex p)) (+ y (getIValue ejey p)) 0));Backtraking en caso de que no se encuentre solucion actualiza la matriz en el punto que se esta  verificando y lo vuelve 0 para devolverse y recalcular
                                     (aux-Sol n ncuad myMatrix  mov  x   y  limit ejex ejey (+ p 1) 0 ) ; aumenta el contador p en 1 para acceso al ejex y ejey de movimiento
                                                     
                                     )
                 (else (aux-Sol n ncuad myMatrix mov 0 0  limit ejex ejey 8 1 )))  ;si ya se abarco la solucion completa se sale del ciclo de p y actualiza sol en 1 
                       )
                      (else (aux-Sol n ncuad myMatrix  mov  x   y  limit ejex ejey (+ p 1) 0 )))) ; si no se ha llegado la solucion aumenta el contador p para abarcar otro movimiento en los ejex, ejey
         

         
         ))
;------------------------------------------------------------------PDC-Todas---------------------------------------------------------------------------------------------
(define(PDC-Todas n lista) ;Invoca la aux para tener control del los movimientos
  (setMatrix (generateMatrixIJ n 0 '())) ; genera la matriz con indices en 0
  (aux-Sol3 n (* n n) myMatrix 1 (first lista) (second lista) (- n 1) '(-1 -2 -2 -1 1 2 2 1) '(-2 -1 1 2 2 1 -1 -2) 0 0))

(define (aux-Sol3 n ncuad matrix mov x y limit ejex ejey p sol) ;auxiliar
  (cond ((equal? mov 1) (aux-Sol3 n ncuad (setMatrix (updateValueIJ myMatrix x y mov)) (+ mov 1) x y limit ejex ejey p sol )) ;verifica el primer movimiento y lo anota en la matriz y aumenta el mov en 1
        ((equal? sol 1)(printMatrix (completeMatrix myMatrix '() n) n)) ; verifica si existe solucion, esto se da cuando sol es igual a 1
        ((< p 8); verifica si se recorrio todo el ejex y el ejey
                (cond ((and(and(and(>= (+ x (getIValue ejex p)) 0) (<=(+ x (getIValue ejex p)) limit)) (and(>= (+ y (getIValue ejey p)) 0) (<=(+ y (getIValue ejey p)) limit))) (equal? (getIJValue myMatrix (+ x (getIValue ejex p)) (+ y (getIValue ejey p)) ) 0)); verifica si la nueva x y, son moviminetos validos, si puede moverse ahi solo si en ese indice es 0
                (cond ((< mov ncuad) (aux-Sol3 n ncuad (setMatrix(updateValueIJ myMatrix (+ x (getIValue ejex p)) (+ y (getIValue ejey p)) mov)) (+ mov 1) (+ x (getIValue ejex p)) (+ y (getIValue ejey p)) limit ejex ejey 0 0 ); verifica si se han abarcado todos los movimientos ncuad, si no entonces actualiza el nuevo x y, y devuelve el mov aumentado en 1
                                
                                     (setMatrix(updateValueIJ myMatrix (+ x (getIValue ejex p)) (+ y (getIValue ejey p)) 0)) ;Backtraking en caso de que no se encuentre solucion actualiza la matriz en el punto que se esta  verificando y lo vuelve 0 para devolverse y recalcular

                                     (aux-Sol3 n ncuad myMatrix  mov  x   y  limit ejex ejey (+ p 1) 0 ) ; aumenta el contador p en 1 para acceso al ejex y ejey de movimiento
                                                      
                                     )
                 (else (aux-Sol3 n ncuad myMatrix mov 0 0  limit ejex ejey 8 1 ))) ;si ya se abarco la solucion completa se sale del ciclo de p y actualiza sol en 1 
                       )
                      (else (aux-Sol3 n ncuad myMatrix  mov  x   y  limit ejex ejey (+ p 1) 0 )))); si no se ha llegado la solucion aumenta el contador p para abarcar otro movimiento en los ejex, ejey
         
         ))
;------------------------------------------------------------------PDC-Test----------------------------------------------------------------------------------------------
(define(PDC-Test n sol) ;Invoca la aux para tener control del los movimientos
  (aux-Sol2 (* n n) 1 sol sol (first (findStart sol 0 0 #f) ) (second (findStart sol 0 0 #f)) (- n 1)));auxiliar para ejecución

(define (aux-Sol2 ncuad mov matrix matrixaux x y limit)
 
  (cond ((equal? mov 1) (aux-Sol2 ncuad (+ mov 1) (updateValueIJ matrix x y 0) matrixaux x y limit));verifica el primer movimiento y lo anota en la matriz y aumenta el mov en 1
      ((< mov ncuad) (cond              ;verifica que aun no se haya llegado al movimiento final
      ((and(and(and(>= (- x 2) 0) (<=(- x 2) limit)) (and(>= (- y 1) 0) (<=(- y 1) limit))) (equal? (getIJValue matrix (- x 2) (- y 1) ) mov)  ) (aux-Sol2 ncuad (+ mov 1) (updateValueIJ matrix (- x 2) (- y 1) 0) matrixaux (- x 2) (- y 1) limit));verifica si la posicion actualizada es valida si lo es aumenta el movimiento en 1
      ;___________________________________________________________________________________________________________________________________________________________________________________________________________________________________
      ((and(and(and(>= (- x 2) 0) (<=(- x 2) limit)) (and(>= (+ y 1) 0) (<=(+ y 1) limit))) (equal? (getIJValue matrix (- x 2) (+ y 1) ) mov)  ) (aux-Sol2 ncuad (+ mov 1) (updateValueIJ matrix (- x 2) (+ y 1) 0) matrixaux (- x 2) (+ y 1) limit));verifica si la posicion actualizada es valida si lo es aumenta el movimiento en 1
      ;___________________________________________________________________________________________________________________________________________________________________________________________________________________________________
      ((and(and(and(>= (- x 1) 0) (<=(- x 1) limit)) (and(>= (+ y 2) 0) (<=(+ y 2) limit))) (equal? (getIJValue matrix (- x 1) (+ y 2) ) mov)  ) (aux-Sol2 ncuad (+ mov 1) (updateValueIJ matrix (- x 1) (+ y 2) 0) matrixaux (- x 1) (+ y 2) limit));verifica si la posicion actualizada es valida si lo es aumenta el movimiento en 1
      ;___________________________________________________________________________________________________________________________________________________________________________________________________________________________________
      ((and(and(and(>= (+ x 1) 0) (<=(+ x 1) limit)) (and(>= (+ y 2) 0) (<=(+ y 2) limit))) (equal? (getIJValue matrix (+ x 1) (+ y 2) ) mov)  ) (aux-Sol2 ncuad (+ mov 1) (updateValueIJ matrix (+ x 1) (+ y 2) 0) matrixaux (+ x 1) (+ y 2) limit));verifica si la posicion actualizada es valida si lo es aumenta el movimiento en 1
      ;___________________________________________________________________________________________________________________________________________________________________________________________________________________________________
      ((and(and(and(>= (+ x 2) 0) (<=(+ x 2) limit)) (and(>= (+ y 1) 0) (<=(+ y 1) limit))) (equal? (getIJValue matrix (+ x 2) (+ y 1) ) mov)  ) (aux-Sol2 ncuad (+ mov 1) (updateValueIJ matrix (+ x 2) (+ y 1) 0) matrixaux (+ x 2) (+ y 1) limit));verifica si la posicion actualizada es valida si lo es aumenta el movimiento en 1
      ;___________________________________________________________________________________________________________________________________________________________________________________________________________________________________
      ((and(and(and(>= (+ x 2) 0) (<=(+ x 2) limit)) (and(>= (- y 1) 0) (<=(- y 1) limit))) (equal? (getIJValue matrix (+ x 2) (- y 1) ) mov)  ) (aux-Sol2 ncuad (+ mov 1) (updateValueIJ matrix (+ x 2) (- y 1) 0) matrixaux (+ x 2) (- y 1) limit));verifica si la posicion actualizada es valida si lo es aumenta el movimiento en 1
      ;___________________________________________________________________________________________________________________________________________________________________________________________________________________________________
      ((and(and(and(>= (+ x 1) 0) (<=(+ x 1) limit)) (and(>= (- y 2) 0) (<=(- y 2) limit))) (equal? (getIJValue matrix (+ x 1) (- y 2) ) mov)  ) (aux-Sol2 ncuad (+ mov 1) (updateValueIJ matrix (+ x 1) (- y 2) 0) matrixaux (+ x 1) (- y 2) limit));verifica si la posicion actualizada es valida si lo es aumenta el movimiento en 1
      ;___________________________________________________________________________________________________________________________________________________________________________________________________________________________________
      ((and(and(and(>= (- x 1) 0) (<=(- x 1) limit)) (and(>= (- y 2) 0) (<=(- y 2) limit))) (equal? (getIJValue matrix (- x 1) (- y 2) ) mov)  ) (aux-Sol2 ncuad (+ mov 1) (updateValueIJ matrix (- x 1) (- y 2) 0) matrixaux (- x 1) (- y 2) limit));verifica si la posicion actualizada es valida si lo es aumenta el movimiento en 1
      (else (display '(NO ES PARTE DE LAS MULTIPLES SOLUCIONES PARA LA POSICION)) (display (findStart matrixaux 0 0 #f))) ;si no se cumple ninguno de los 8 movimientos quiere decir que la solucion no es valida
                       ))
      (else (printMatrix matrixaux(+ 1 limit) )))) ; si mov llega a ser = a ncuad quiere decir que el recorrido termino y se imprime la solucion

 ;--------------------------------------------------------------------------------TEST-------------------------------------------------------------------------------------------------------------------------------     


;(PDC-Todas 5 '(0 0))
;(PDC-Test 5 '((21 18 9 4 1) (8 3 20 17 10) (19 22 13 2 5) (14 7 24 11 16) (23 12 15 6 25)))
;(PDC-Paint 6 '((26 13 4 9 28 11) (3 32 27 12 5 8) (14 25 2 7 10 29) (33 22 31 18 1 6) (24 15 20 35 30 17) (21 34 23 16 19 36)))
(PDC-Paint 5 '((1 16 11 6 3) (10 5 2 17 12) (15 22 19 4 7) (20 9 24 13 18) (23 14 21 8 25)))

