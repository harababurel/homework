(defun linearize (xs)
    ;(format t "~d~%" xs)
    (cond
        ((null xs) nil)
        ((atom xs) (list xs))
        (t (apply 'append (mapcar 'linearize xs)))
    )
)

(defun lastElement (xs)
    (cond
        ((eq 1 (length xs)) (car xs))
        (t (lastElement (cdr xs)))
    )
)

(defun lastAtom (xs)
    (lastElement (linearize xs))
)

(defun solve (xs)
    (format t "solving ~d~%" xs)
    (cond
        ((atom xs) 0)
        ((null xs) 0)
        (t (+ (cond
                ((not (numberp (lastAtom xs))) 1)
                (t 0)
              )
              (apply '+ (mapcar 'solve xs))
           )
        )
    )
)


(format t "result: ~d~%" (linearize (list 1 2 (list 3 4 (list 5) 6))))
(format t "result: ~d~%" (linearize (list 1 2 (list 3 4 '() 6))))

(format t "solve: ~d~%" (solve (list 'A (list 'B 2) (list 1 'C 4) (list 'D 1 (list 6 'F)) (list (list 'G 4) 6) 'F)))
