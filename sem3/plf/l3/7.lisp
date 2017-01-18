; 7. Write a function that substitutes an element E with all elements of a list L1 at all levels of a given list L.

;(defun substituteOne (xs x ys)
;    (cond
;        ((null xs) '())
;        ((eq (first xs) x) (append ys (substituteOne (cdr xs) x ys)))
;        (t (append (list (first xs)) (substituteOne (cdr xs) x ys)))
;    )
;)

; substituteOne((l1, l2, ... ln), x, (y1, y2, ..., ym)) =
;   empty set,                                                      n = 0
;   l1,                                                             n = 1 and l1 =/= x
;   (y1, y2, ..., ym),                                              n = 1 and l1 = x
;   UNION({substituteOne(li, x, (y1, y1, ..., ym)) | 1 <= i <= n}), otherwise
(defun substituteOne (xs x ys)
    (cond
        ((null xs) nil)
        ((atom xs) (cond ((eq xs x) ys)
                         (t xs)))
        (t (append (mapcar (lambda (e) (substituteOne e x ys)) xs)))
    )
)

; substituteAll((l1, l2, ..., ln), x (y1, y2, ..., ym)) =
;    empty set,                                                      n = 0
;    substituteOne(l1, x, (y1, y2, ..., ym)),                        n = 1
;    UNION({substituteAll(li, x, (y1, y2, ..., ym)) | 1 <= i <= n}), otherwise
(defun substituteAll (xs x ys)
    (cond
        ((null xs) nil)
        ((atom xs) (substituteOne xs x ys))
        (t (append (mapcar (lambda (e) (substituteAll e x ys)) xs)))
    )
)

(format t "~D~%" (substituteOne '(1 2 3 2 5) 2 '(6 7 8)))
(format t "~D~%" (substituteAll '(1 2 3 (2 4 (2) (2)) 5) 2 '(0 0)))
(format t "~D~%" (substituteAll '(1 2 3 (2 4 (2) (2)) 5) 1 '(0 0)))

