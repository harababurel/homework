; nonNumericAtoms(l1, l2, ..., ln) = { empty set,                             n  = 0
;                                    { l1 U nonNumericAtoms(l2, l3, ..., ln), l1 = nonnumeric atom
;                                    { nonNumericAtoms(l2, l3, ..., ln),      otherwise
(defun nonNumericAtoms (xs)
    (cond
        ((null xs) '())
        ((and (atom (first xs)) (not (numberp (first xs))))
            (append (list (first xs)) (nonNumericAtoms (cdr xs))))
        (t (nonNumericAtoms (cdr xs)))
    )
)


;(defun generatePairWith (x xs)
;    (cond
;        ((eq (length xs) 1) (list x (first xs)))
;        (t (append (list x (first xs)) (generatePairWith x (cdr xs))))
;    )
;)

; generatePairsWith(e, (l1, l2, ..., ln)) = {(e, li) | 1 <= i <= n}
(defun generatePairsWith (x xs)
    (mapcar (lambda (e) (list x e)) xs)
)
; generateAllPairs(l1, l2, ..., ln) = {(x, y) | (x, y) belong to UNION(generatePairsWith(li, (l1, l2, ..., ln)) | 1 <= i <= n),
;                                               index(x, (l1, l2, ..., ln)) < index(y, (l1, l2, ..., ln))}
(defun generateAllPairs (xs)
    (remove-if (lambda (x) (>= (position (first x) xs) (position (cadr x) xs)))
        (apply 'append (mapcar (lambda (e) (generatePairsWith e xs)) xs))
    )
)

; solve(l1, l2, ..., ln) = generateAllPairs(nonNumericAtoms(l1, l2, ..., ln))
(defun solve (xs)
    (generateAllPairs (nonNumericAtoms xs))
)

;(format t "~D~%" (nonNumericAtoms '(A 2 B 3 C D 1)))
;(format t "~D~%" (generatePairsWith 'A '(A 2 B 3 C D 1)))
(format t "~D~%" (solve '(A 2 B 3 C D 1)))

;(format t "~D~%" (solve '(A B C D E F G H I J K L)))
