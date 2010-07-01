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

(defun eliminar_nils (l)
	(if (null l)
		nil
		(if (null (car l))
			(eliminar_nils (cdr l))
			(cons (car l) (eliminar_nils (cdr l)))
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
;si existe devuelve nil
(defun agregar_nodo_si_no_existe (path nodo)
	(if (contiene nodo path)
		nil
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
	(eliminar_nils (expandir_camino_rep path adyacencias))
)

(defun expandir_caminos (paths adyacencias)
	(eliminar_nils (expandir_caminos_rep paths adyacencias))
)

;paths = ( (1 2 3 ) (1 3 4) ....)
;adyacencias = ( 3 4 5 )
(defun expandir_caminos_rep (paths adyacencias)
	(if (null paths)
		nil
		(append (expandir_camino (car paths) adyacencias) (expandir_caminos (cdr paths) adyacencias))
	)
)

(defun find_path (source target adyacencias)
	(do_find_path source target adyacencias (list (list source)))
)

(defun do_find_path (source target adyacencias path)
	(if (or (null source) (null target) (null adyacencias))
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

(test 'params (find_path nil nil nil) nil)

;adyacencias
(test 'ady1 (adyacencias_de 'a '((a (b c))) ) '(b c))
(test 'ady2 (adyacencias_de 'a '((d (b c))) ) nil)
(test 'ady3 (adyacencias_de 'a '((b (a c)) (a (b c))) ) '(b c))

(test 'expandir1 (expandir_camino_rep '(1) '(2 3) ) '((1 2) (1 3)))
(test 'expandir2 (expandir_camino_rep '(1 4) '(2 3) ) '((1 4 2) (1 4 3)))
(test 'expandir3 (expandir_camino_rep '(1 4) nil) '((1 4)))
(test 'expandir4 (expandir_camino_rep '(1) '(2 3 5) ) '((1 2) (1 3) (1 5)))
(test 'expandir5 (expandir_camino_rep '(1) '(1 3 5) ) '(nil (1 3) (1 5)))
(test 'expandir6 (expandir_camino_rep '(1 2) '(1 3 5) ) '(nil (1 2 3) (1 2 5)))
(test 'expandir7 (expandir_camino_rep '(1 2) '(1 2 5) ) '(nil nil (1 2 5)))

(test 'expandir8 (expandir_camino '(1 2) '(1 2 5) ) '((1 2 5)))
(test 'expandir9 (expandir_camino '(1 2) '(1 3 5) ) '((1 2 3) (1 2 5)))

(test 'expandir10 (expandir_caminos '((1 2)) '(1 3 5) ) '((1 2 3) (1 2 5)))
(test 'expandir11 (expandir_caminos '((1 2) (1)) '(1 3 5) ) '((1 2 3) (1 2 5) (1 3) (1 5)))
(test 'expandir12 (expandir_caminos '((1 2)) '() ) '((1 2)))

(test 'contiene1 (contiene '1 '(1 2 3)) t)
(test 'contiene2 (contiene '1 '(9 8 1 2 3)) t)
(test 'contiene3 (contiene '1 '(9 8 2 2 3)) nil)

(test 'agregar_nodo1 (agregar_nodo_si_no_existe '(1 2 3) '4) '(1 2 3 4))
(test 'agregar_nodo2 (agregar_nodo_si_no_existe '(1 2 3) '3) nil)
(test 'agregar_nodo3 (agregar_nodo_si_no_existe '(1 3 2) '3) nil)

(test 'elim_nils (eliminar_nils '(1 3 2 nil)) '(1 3 2))
(test 'elim_nils2 (eliminar_nils '(1 3 2 nil)) '(1 3 2))
(test 'elim_nils3 (eliminar_nils '(1 nil 3 2 nil)) '(1 3 2))
(test 'elim_nils4 (eliminar_nils '(nil nil 1 nil 3 2 nil nil nil 1)) '(1 3 2 1))
(test 'elim_rep1 (eliminar_repetidos '(1 3 2)) '(1 3 2))
(test 'elim_rep2 (eliminar_repetidos '(1 3 2 2)) '(1 3 2))
(test 'elim_rep3 (eliminar_repetidos nil) nil)
(test 'elim_rep4 (eliminar_repetidos '(1 2 3 2 2)) '(1 3 2))
