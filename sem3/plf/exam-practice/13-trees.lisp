; http://www.cs.ubbcluj.ro/~hfpop/teaching/2016/pfl/lab/l2.pdf
; 13. For a given tree of type (2) return the path from the root node to a certain given node X.

;  A
; / \
; B C
;  / \
;  D E
;
; (A (B) (C (D) (E)))

(defun explore (tree target)
    (setq node (first tree) children (cdr tree))
    ;(format t "entered node ~d~%" node)

    ;(format t "node: ~D~%" node)
    ;(format t "children: ~D~%" children)

    (cond
        ((eq node target)
            ;(format t "am gasit nodu target fam~%")
            (list node)
        )
        (t
            (setq nextHop (car (remove nil (mapcar (lambda (subtree) (when (contains subtree target) subtree)) children))))
            ;(format t "nextHop: ~d~%" nextHop)

            (cond
                ((null nextHop) (format t "Target does not exist.~%"))
                (t (append (list (first tree)) (explore nextHop target)))
            )
        )
    )

)

(defun contains (tree target)
    (setq node (first tree) children (cdr tree))
    (cond
        ((eq node target) t)
        (t (find t (mapcar (lambda (subtree) (contains subtree target)) children)))
    )
)
; TODO: ^solve without this

(format t "~d~%" (explore '(A (B) (C (D) (E))) 'D))
