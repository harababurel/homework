(defun removeElement (xs x)
    ;(format t "will remove ~d from ~d~%" x xs)
    (cond
        ((null xs) ())
        ((listp (first xs)) ;(format t "head is list. going deeper~%")
                            (append (list (removeElement (first xs) x)) (removeElement (cdr xs) x)))
        ((eq x (first xs)) ;(format t "found occurrence~%")
                           (removeElement (cdr xs) x))
        (t ;(format t "nothing found~%")
           (append (list (first xs)) (removeElement (cdr xs) x)))
    )
    ;(when (not (null (cdr xs))) (removeElement (cdr xs) x))
)

(defun removeElementMap (xs x)
    (cond
        ((atom xs) (when (not (eq x xs)) xs))
        (t (mapcar (lambda (e) (removeElementMap e x)) xs))))


;(format t "result: ~d~%" (removeElement (list 1 2 (list 2 3) (list 2) 3 ) 2))
(format t "no map:  ~d~%" (removeElement (list 1 (list 2 'A (list 3 'A)) (list 'A)) 'A))
(format t "yes map: ~d~%" (removeElementMap (list 1 (list 2 'A (list 3 'A)) (list 'A)) 'A))

(format t "~%")
(format t "no map:  ~d~%" (removeElement (list 1 (list 2 (list 3))) 'A))
(format t "yes map: ~d~%" (removeElementMap (list 1 (list 2 (list 3))) 'A))
