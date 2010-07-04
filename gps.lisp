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
	(do_expandir_caminos paths adyacencias (expandir_caminos_rep paths adyacencias))
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

;busca los caminos entre source y target
;source: nodo origen
;target: nodo destino
;adyacencias: lista de adyacencias
(defun find_path (source target adyacencias)
	(do_find_path source target adyacencias (list (list source)))
)

;busca los caminos entre source y target
;path es el listado de caminos que recorrio ( (1 2) (1 3) ... )
;adyacencias: lista de adyacencias
;
;intenta expandir la lista de caminos en base a las adyacencias
;si no hay mas posibles caminos, termina la ejecucion
(defun do_find_path (source target adyacencias path)
	(if (or (null source) (null target) (null adyacencias))
		nil
		(seguir_buscando_si_hay_caminos source target adyacencias path (expandir_caminos path adyacencias))
	)
)

;sigue buscando caminos si entre path y new path son distintos
;new path es la expansion de los caminos segun las adyacencias actuales
(defun seguir_buscando_si_hay_caminos (source target adyacencias path new_path)
	(if (equal path (eliminar_repetidos new_path))
		path
		(do_find_path source target adyacencias new_path)
	)
)

;testing function
;=============================
(defun test (name got expected)
	(if (equal expected got)
		;t
		(progn (print '==ok==) (print name))
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

(test 'params (find_path nil nil nil) nil)

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
(test 'find_caminos1 (find_path '1 '5 '((1 (5)) (5 (1)))) '((1 5)))

;1 - 2
;|-3-|
(test 'expcam1 (expandir_caminos '((1 2 3) (1 3 2)) '((1 (2 3)) (2 (1 3)) (3 (1 2)))) '((1 2 3) (1 3 2)))
(test 'find_caminos2 (find_path '1 '3 '((1 (2 3)) (2 (1 3)) (3 (1 2)))) '((1 2)(1 2 3)(1 3)(1 3 2)))
