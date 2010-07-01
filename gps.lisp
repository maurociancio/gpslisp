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
