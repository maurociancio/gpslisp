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
