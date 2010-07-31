;autor mauro ciancio

;====================================================================
;manejos gral de listas
;====================================================================

;e elemento
;l lista
;retorna si e existe en l
(defun contiene (e l)
    (if (null l)
        nil
        (if (equal e (car l))
            t
            (contiene e (cdr l))
        )
    )
)

(defun eliminar_repetidos (l)
    (if (null l)
        nil
        (if (contiene (car l) (cdr l))
            (eliminar_repetidos (cdr l))
            (cons (car l) (eliminar_repetidos (cdr l)))
        )
    )
)

;====================================================================
;adyacencias
;====================================================================

;adyacencias
;((nodo1 (nodo2 nodo3)) (nodo2 (nodo1) ...))
(defun adyacencias_de (nodo adyacencias)
    (if (or (null nodo) (null adyacencias))
        nil
        (if (eq nodo (caar adyacencias))
            (cadar adyacencias)
            (adyacencias_de nodo (cdr adyacencias))
        )
    )
)

;agrega el nodo nodo al path si no existe en el camino.
;si existe devuelve nil
(defun add_nodo_a_path (path nodo)
    (if (contiene nodo path)
        path
        (append path (list nodo))
    )
)

;====================================================================
;manejo de 1 camino
;====================================================================

;path (nodo1 nodo2 nodo3 ...)
;adyacencias (nodox nodoy ...)
;crea tantos caminos como adyacencias no incluidas en el camino actual
;devuelve con duplicados
;ejemplo
;path (1)
;adyacencias (2 3)
;devuelve ((1 2) (1 3))
(defun expandir_camino_rep (path adyacencias)
    (if (null adyacencias)
        (list path)
        (mapcar (lambda (e) (add_nodo_a_path path e)) adyacencias)
    )
)

;idem anterior pero sacando repetidos
(defun expandir_camino (path adyacencias)
    (eliminar_repetidos (expandir_camino_rep path adyacencias))
)

;====================================================================
;manejo de varios caminos
;====================================================================

(defun expandir_caminos (paths adyacencias)
    (eliminar_repetidos (do_expandir_caminos paths adyacencias (expandir_caminos_rep paths adyacencias)))
)

(defun do_expandir_caminos (paths adyacencias newpaths)
    (if (null newpaths)
        paths
        newpaths
    )
)

;paths = ( (1 2 3 ) (1 3 4) ....)
;adyacencias = ( (1 (3 4 5 )) ( 2 ( 3 4 5)))
(defun expandir_caminos_rep (paths adyacencias)
    (if (null paths)
        nil
        (append (expandir_camino (car paths) (adyacencias_de (car (last (car paths))) adyacencias))
            (expandir_caminos (cdr paths) adyacencias))
    )
)

;====================================================================
;busqueda de los caminos
;====================================================================

;busca todos los caminos alcanzables desde el source
;source: nodo origen
;adyacencias: lista de adyacencias
(defun find_path (source adyacencias)
    (do_find_path adyacencias (list (list source)))
)

;busca los caminos alcanzables desde el path
;path es el listado de caminos que recorrio ( (1 2) (1 3) ... )
;adyacencias: lista de adyacencias
;
;intenta expandir la lista de caminos en base a las adyacencias
;si no hay mas posibles caminos, termina la ejecucion
(defun do_find_path (adyacencias path)
    (if (null adyacencias)
        nil
        (seguir_buscando_si_hay_caminos adyacencias path (expandir_caminos path adyacencias))
    )
)

;sigue buscando caminos si entre path y new path son distintos
;new path es la expansion de los caminos segun las adyacencias actuales
(defun seguir_buscando_si_hay_caminos (adyacencias path new_path)
    (if (equal path new_path)
        path
        (do_find_path adyacencias new_path)
    )
)

;====================================================================
;filtrado de los caminos
;====================================================================

;paths: listado de caminos ( (1 2 3) (1 3 2 4) ...))
;target: destino
(defun filtrar_caminos (target paths)
    (if (null paths)
        nil
        (if (eq (car (last (car paths))) target)
            (cons (car paths) (filtrar_caminos target (cdr paths)))
            (filtrar_caminos target (cdr paths))
        )
    )
)

;busca todos los caminos entre origen y destino, en base a una lista de adyacencias
(defun caminos_entre (source target adyacencias)
    (filtrar_caminos target (find_path source adyacencias))
)

(defun is_max (new old)
    (> new old)
)

(defun is_min (new old)
    (< new old)
)

;busca la longitud del camino mas largo
(defun long (caminos f &optional (max nil))
    (if (null caminos)
        max
        (if (null max)
            (long (cdr caminos) f (length (car caminos)))
            (long (cdr caminos) f
                (if (funcall f (length (car caminos)) max)
                    (length (car caminos))
                     max
                )
            )
        )
    )
)

;devuelve el camino de mayor longitud y el camino de menor longitud
(defun max_min (caminos &optional (minimos nil) (maximos nil) (min nil) (max nil))
    (if (null caminos)
        (list minimos maximos)

        (if (or (null min) (null max))
            (max_min caminos minimos maximos (long caminos 'is_min) (long caminos 'is_max))
            (cond
                ;el camino actual es maximo y minimo
                ((and (= min (length (car caminos))) (= max (length (car caminos))))
                (max_min (cdr caminos) (cons (car caminos) minimos) (cons (car caminos) maximos) min max)
                )
                ;solo min
                ((= min (length (car caminos)))
                (max_min (cdr caminos) (cons (car caminos) minimos) maximos min max)
                )
                ;solo max
                ((= max (length (car caminos)))
                (max_min (cdr caminos) minimos (cons (car caminos) maximos) min max)
                )
                ;nada
                (t
                (max_min (cdr caminos) minimos maximos min max)
                )
            )
        )
    )
)

;====================================================================
;traduccion de nodos a caminos
;====================================================================

;nodo: nodo
;cruces: ( ( nodo (id1 id2)) (nodo2 (id2 id3)) ...)
(defun buscar_cruce (nodo cruces)
    (if (null cruces)
        nil
        (if (eq (caar cruces) nodo)
            (cadar cruces)
            (buscar_cruce nodo (cdr cruces))
        )
    )
)

;path (nodo1 nodo2 ... nodo3)
;cruces: ( ( nodo (id1 id2)) (nodo2 (id2 id3)) ...)
(defun traducir_a_cruces (path cruces)
    (if (null path)
        nil
        (cons (buscar_cruce (car path) cruces) (traducir_a_cruces (cdr path) cruces))
    )
)

(defun buscar_calle (calle mapa)
    (if (null mapa)
        nil
        (if (eq (caar mapa) calle)
            (cadar mapa)
            (buscar_calle calle (cdr mapa))
        )
    )
)

;cruces ( (calle1 calle2) (calle 2 calle 4) ...)
;mapa ( (1 calle) (2 calle2) ...)
(defun traducir_a_calles (cruces mapa)
    (if (null cruces)
        nil
        (cons (list (buscar_calle (caar cruces) mapa) (buscar_calle (cadar cruces) mapa))
            (traducir_a_calles (cdr cruces) mapa))
    )
)

(defun interseccion (esq1 esq2)
    (cond
        ((equal (nth 0 esq1) (nth 0 esq2)) (nth 0 esq1))
        ((equal (nth 0 esq1) (nth 1 esq2)) (nth 0 esq1))
        ((equal (nth 1 esq1) (nth 0 esq2)) (nth 1 esq1))
        ((equal (nth 1 esq1) (nth 1 esq2)) (nth 1 esq1))
    )
)

(defun comprimir_recorrido (recorrido &optional (prev nil))
    (if (null recorrido)
        nil
        (if (null prev)
            (comprimir_recorrido (cdr recorrido) (car recorrido))
            (cons (interseccion prev (car recorrido)) (comprimir_recorrido (cdr recorrido) (car recorrido)))
        )
    )
)

(defun juntar_iguales (recorrido &optional (actual nil) (cant nil))
    (if (or (null actual) (null cant))
        (if (null recorrido)
            nil
            (juntar_iguales (cdr recorrido) (car recorrido) 1)
        )
        (if (null recorrido)
            (list (list actual cant))
            (if (eq (car recorrido) actual)
                (juntar_iguales (cdr recorrido) actual (+ cant 1))
                (cons (list actual cant) (juntar_iguales (cdr recorrido) (car recorrido) 1))
            )
        )
    )
)

(defun describir_camino (camino)
    (print 'hacer)
    (print (nth 1 (car camino)))
    (print '(cuadras por))
    (print (caar camino))
    (describir_camino2 (cdr camino))
)
(defun describir_camino2 (camino)
    (if (null camino)
        (print '(hasta arribar al punto de llegada gracias!))
        (progn
            (print '(doblar en))
            (print (caar camino))
            (print '(hacer))
            (print (nth 1 (car camino)))
            (print '(cuadras))
            (describir_camino2 (cdr camino))
        )
    )
)

;testing function
;=============================
(defun test (name got expected)
    (if (equal expected got)
        t
        ;(progn (print '==ok==) (print name))
        (progn (print '==error==) (print name) (print 'expected) (print expected) (print 'got) (print got))
    )
)
;=============================
;===traces===
(defun do_trace ()
    (trace add_nodo_a_path)
    (trace do_expandir_caminos)
    (trace expandir_caminos_rep)
    (trace expandir_camino)
    (trace expandir_caminos)
    (trace adyacencias_de)
    (trace do_find_path)
    (trace seguir_buscando_si_hay_caminos)
    (trace expandir_caminos)
)

(test 'params (find_path nil nil) nil)

;adyacencias
(test 'ady1 (adyacencias_de 'a '((a (b c))) ) '(b c))
(test 'ady2 (adyacencias_de 'a '((d (b c))) ) nil)
(test 'ady3 (adyacencias_de 'a '((b (a c)) (a (b c))) ) '(b c))

(test 'expandir0 (expandir_caminos_rep '((1 3)) '((1 (2 3)) (2 (1 3)) (3 (1 2)))) '((1 3)(1 3 2)))

(test 'repetidos (eliminar_repetidos '((1 2 3) (1 2 3) (1 3 2) (1 3 2))) '((1 2 3)(1 3 2)))

(test 'expandir1 (expandir_camino_rep '(1) '(2 3) ) '((1 2) (1 3)))
(test 'expandir2 (expandir_camino_rep '(1 4) '(2 3) ) '((1 4 2) (1 4 3)))
(test 'expandir3 (expandir_camino_rep '(1 4) nil) '((1 4)))
(test 'expandir4 (expandir_camino_rep '(1) '(2 3 5) ) '((1 2) (1 3) (1 5)))
(test 'expandir5 (expandir_camino_rep '(1) '(1 3 5) ) '((1) (1 3) (1 5)))
(test 'expandir6 (expandir_camino_rep '(1 2) '(1 3 5) ) '((1 2) (1 2 3) (1 2 5)))
(test 'expandir7 (expandir_camino_rep '(1 2) '(1 2 5) ) '((1 2)(1 2) (1 2 5)))

(test 'expandir8 (expandir_camino '(1 2) '(1 2 5) ) '((1 2)(1 2 5)))
(test 'expandir9 (expandir_camino '(1 2) '(1 3 5) ) '((1 2)(1 2 3) (1 2 5)))
(test 'expandir10 (expandir_caminos '((1 2)) '((1 (3 5)) (2 (3 5))) ) '((1 2 3) (1 2 5)))
(test 'expandir11 (expandir_caminos '((1 2) (1)) '((1 (3 5)) (2 (3 5))) ) '((1 2 3) (1 2 5) (1 3) (1 5)))
(test 'expandir12 (expandir_caminos '((1 2)) '() ) '((1 2)))

(test 'expandir13 (expandir_caminos '((1 2)) '( (2 (3)) ) ) '((1 2 3)))
(test 'expandir14 (expandir_caminos '((1 5)) '((1 (5)) (5 (1))) ) '((1 5)))
(test 'expandir15 (expandir_caminos '((1 2) (1 3)) '((1 (2 3)) (2 (1 3)) (3 (1 2))) ) '((1 2)(1 2 3)(1 3)(1 3 2)))

(test 'contiene1 (contiene '1 '(1 2 3)) t)
(test 'contiene2 (contiene '1 '(9 8 1 2 3)) t)
(test 'contiene3 (contiene '1 '(9 8 2 2 3)) nil)

(test 'agregar_nodo1 (add_nodo_a_path '(1 2 3) '4) '(1 2 3 4))
(test 'agregar_nodo2 (add_nodo_a_path '(1 2 3) '3) '(1 2 3))
(test 'agregar_nodo3 (add_nodo_a_path '(1 3 2) '3) '(1 3 2))

(test 'elim_rep1 (eliminar_repetidos '(1 3 2)) '(1 3 2))
(test 'elim_rep2 (eliminar_repetidos '(1 3 2 2)) '(1 3 2))
(test 'elim_rep3 (eliminar_repetidos nil) nil)
(test 'elim_rep4 (eliminar_repetidos '(1 2 3 2 2)) '(1 3 2))

;1 - 5
(test 'find_caminos1 (find_path '1 '((1 (5)) (5 (1)))) '((1 5)))

;1 - 2
;|-3-|
(test 'expcam1 (expandir_caminos '((1 2 3) (1 3 2)) '((1 (2 3)) (2 (1 3)) (3 (1 2)))) '((1 2 3) (1 3 2)))
(test 'find_caminos2 (find_path '1 '((1 (2 3)) (2 (1 3)) (3 (1 2)))) '((1 2)(1 2 3)(1 3)(1 3 2)))
(test 'find_caminos_filtrados
    (filtrar_caminos '3 (find_path '1 '((1 (2 3)) (2 (1 3)) (3 (1 2)))))
    '((1 2 3)(1 3))
)

;test maximos-minimos

(test 'long_max1 (long '( (1) (1)  ) 'is_max ) '1)
(test 'long_max2 (long '( (1 2)  )  'is_max '1) '2)
(test 'long_max3 (long '( (1) (1 2)  ) 'is_max ) '2)
(test 'long_max4 (long '( (1 2)  ) 'is_max ) '2)
(test 'long_max5 (long '( (1) (1 2) ( 1 2 3 4) ) 'is_max ) '4)
(test 'long_min1 (long '( (1) (1 2) ( 1 2 3 4) ) 'is_min ) '1)
(test 'long_min2 (long '( (1 2) ( 1 2 3 4) ) 'is_min ) '2)

(test 'max_min (max_min '( (1) (1 2) ( 1 2 3 4) )) '(((1))((1 2 3 4))))
(test 'max_min2 (max_min '( (1 2) )) '(((1 2))((1 2))))
(test 'max_min3 (max_min '( (1 2) ( 1 2 3 4) )) '(((1 2))((1 2 3 4))))
(test 'max_min4 (max_min '( (1 2) (3 4) ( 1 2 3 4) )) '(((3 4)(1 2))((1 2 3 4))))
(test 'max_min5 (max_min '( (1 2) (3 4) ( 2 3) )) '(((2 3) (3 4) (1 2)) ((2 3) (3 4) (1 2))))

;test traducciones
(test 'buscar_cruce1 (buscar_cruce '1 '((1(2 3)))) '(2 3))
(test 'buscar_cruce2 (buscar_cruce '2 '((1(2 3)))) nil)
(test 'buscar_cruce3 (buscar_cruce '3 '((1(2 3))(3(3 4)))) '(3 4))

(test 'traducir_cruces1 (traducir_a_cruces '(1 3) '( (1(2 3)) (3(3 4)) )) '((2 3)(3 4)))
(test 'traducir_cruces2 (traducir_a_cruces '(1) '( (1(2 3)) (3(3 4)) )) '((2 3)))

(test 'traducir_calles (traducir_a_calles '((1 3)) '((1 3) (3 4))) '((3 4)))
(test 'traducir_calles2 (traducir_a_calles '((1 2)) '((1 3) (2 4))) '((3 4)))
(test 'traducir_calles3 (traducir_a_calles '((1 2)(1 3)) '((1 3) (2 4)(3 5))) '((3 4)(3 5)))

(test 'comprimir (comprimir_recorrido '((1 2)(1 3)(1 4)(1 5)(5 2))) '(1 1 1 5))
(test 'juntar (juntar_iguales '(1 1 2 3)) '((1 2) (2 1) (3 1)))
(test 'juntar2 (juntar_iguales '(1 1 2 3 3)) '((1 2) (2 1) (3 2)))

;test funcional
(test 'fun1 (caminos_entre '1 '6 '( (1 (2 5)) (2 (1 5 3 6)) (3 (2 5)) (5 (2 1 3)) (6 (2)) ))
    '((1 2 6)(1 5 2 6) (1 5 3 2 6))
)
(test 'fun2 (caminos_entre '1 '8 '( (1 (3 2)) (2 (1 4 8)) (3 (1 4)) (4 (3 2 6)) (5 (7 8)) (6 (4 7)) (7 (6 5)) (8 (5 2)) ))
    '((1 3 4 2 8) (1 3 4 6 7 5 8) (1 2 4 6 7 5 8) (1 2 8))
)

;(describir_camino '((indep 2)(pc 10)(9julio 10)))

(setq nodos
        '((a (b f)) (b (a c)) (c (b d)) (d (c e j)) (e (d)) (f(g))
          (g(h)) (h (b i)) (i(c j)) (j(k)) (k(l)) (l(e)))
)

(setq esquinas
        '((a(1 2)) (b(1 5)) (c(1 7)) (d(1 6)) (e(1 9)) (f(4 2))
          (g(2 8)) (h(8 5)) (i(8 7)) (j(8 6)) (k(8 9)) (l(9 4)))
)

(setq calles
        '((1 (Paseo Colon))(2 Independencia)(3 EEUU)(4 Balcarce)(5 Chile)
          (6 Venezuela)(7 Mexico)(8 Defensa)(9 Belgrano))
)

;con el caar obtengo el primer camino minimo, puede haber varios minimos
(print (describir_camino (juntar_iguales (comprimir_recorrido (traducir_a_calles (traducir_a_cruces (caar (max_min
        (caminos_entre 'k 'i  nodos)
)) esquinas) calles)))))
