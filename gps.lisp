;e elemento
;l lista
;retorna si e existe en l
(defun contiene (e l)
	(if (null l)
		nil
		(if (eq e (car l))
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
;si existe devuelve el mismo camino
(defun agregar_nodo_si_no_existe (path nodo)
	(if (contiene nodo path)
		path
		(append path (list nodo))
	)
)

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
		(mapcar (lambda (e) (agregar_nodo_si_no_existe path e)) adyacencias)
	)
)

;idem anterior pero sacando repetidos
(defun expandir_camino (path adyacencias)
	(eliminar_repetidos (expandir_camino_rep path adyacencias))
)

(defun find_path (source target esquinas adyacencias &optional(path '()))
	(if (or (null source) (null target) (null esquinas) (null adyacencias))
		nil
		t
	)
)

;testing function
;=============================
(defun test (name got expected)
	(if (equal expected got)
		t
		(progn (print '==error==) (print name) (print 'expected) (print expected) (print 'got) (print got))
	)
)
;=============================

(test 'params (find_path nil nil nil nil) nil)

;adyacencias
(test 'ady1 (adyacencias_de 'a '((a (b c))) ) '(b c))
(test 'ady2 (adyacencias_de 'a '((d (b c))) ) nil)
(test 'ady3 (adyacencias_de 'a '((b (a c)) (a (b c))) ) '(b c))

(test 'expandir1 (expandir_camino_rep '(1) '(2 3) ) '((1 2) (1 3)))
(test 'expandir2 (expandir_camino_rep '(1 4) '(2 3) ) '((1 4 2) (1 4 3)))
(test 'expandir3 (expandir_camino_rep '(1 4) nil) '((1 4)))
(test 'expandir4 (expandir_camino_rep '(1) '(2 3 5) ) '((1 2) (1 3) (1 5)))
(test 'expandir5 (expandir_camino_rep '(1) '(1 3 5) ) '((1) (1 3) (1 5)))
(test 'expandir6 (expandir_camino_rep '(1 2) '(1 3 5) ) '((1 2) (1 2 3) (1 2 5)))
(test 'expandir7 (expandir_camino_rep '(1 2) '(1 2 5) ) '((1 2) (1 2) (1 2 5)))

(test 'expandir8 (expandir_camino '(1 2) '(1 2 5) ) '((1 2) (1 2 5)))
(test 'expandir9 (expandir_camino '(1 2) '(1 3 5) ) '((1 2) (1 2 3) (1 2 5)))

(test 'contiene1 (contiene '1 '(1 2 3)) t)
(test 'contiene2 (contiene '1 '(9 8 1 2 3)) t)
(test 'contiene3 (contiene '1 '(9 8 2 2 3)) nil)

(test 'agregar_nodo1 (agregar_nodo_si_no_existe '(1 2 3) '4) '(1 2 3 4))
(test 'agregar_nodo2 (agregar_nodo_si_no_existe '(1 2 3) '3) '(1 2 3))
(test 'agregar_nodo3 (agregar_nodo_si_no_existe '(1 3 2) '3) '(1 3 2))

(test 'elim_rep1 (eliminar_repetidos '(1 3 2)) '(1 3 2))
(test 'elim_rep2 (eliminar_repetidos '(1 3 2 2)) '(1 3 2))
(test 'elim_rep3 (eliminar_repetidos nil) nil)
(test 'elim_rep4 (eliminar_repetidos '(1 2 3 2 2)) '(1 3 2))
