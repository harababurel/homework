; 7. Return the level of a node X in a tree of type (1). The level of the root element is 0.

; (node no-subtrees list-subtree-1 list-subtree-2 ...) (1)
; (A 2 B 0 C 2 D 0 E 0) (1)

(defun explore (x node level children xs)
        ;(format t "entered node ~d (level ~d)~%" node level)
        (cond ((eq x node) (format t "found ~d on level ~d~%" x level)))
        (cond
            ((null xs) nil)
            ((= children 0) xs)
            (t (explore x node level (- children 1) (explore x (first xs) (+ level 1) (nth 1 xs) (cdr (cdr xs)))))
        )
)

(defun solve (x xs)
    (explore x (first xs) 1 (nth 1 xs) (cdr (cdr xs))))

;(explore 'B 'A 1 2 (list 'B 0 'C 2 'D 0 'E 0))
;(explore 'E 'A 1 2 (list 'B 0 'C 2 'D 0 'E 0))
;(format t "EX 2:~%")
;(explore 'F 'A 1 4 (list 'B 0 'C 0 'D 1 'F 0 'E 0))


(format t "EX 1:~%")
(solve 'B (list 'A 2 'B 0 'C 2 'D 0 'E 0))
(solve 'E (list 'A 2 'B 0 'C 2 'D 0 'E 0))

(format t "EX 2:~%")
(solve 'F (list 'A 4 'B 0 'C 0 'D 1 'F 0 'E 0))

