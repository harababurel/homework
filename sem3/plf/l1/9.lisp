; 9.
; a) Write a function that merges two sorted linear lists and keeps double values.
; b) Write a function to replace an element E by all elements of a list L1 at all levels of a given list L.
; c) Write a function to determines the sum of two numbers in list representation, and returns the
; corresponding decimal number, without transforming the representation of the number from list to
; number.
; d) Write a function to return the greatest common divisor of all numbers in a linear list.


; a)
; mergeLists (x1 x2 ... xn) (y1 y2 ... ym) = { (y1, y2, ..., ym), n = 0
;                                            { (x2, x2, ..., xn), m = 0
;                                            { x1 union (mergeLists (x2, x3, ..., xn) (y1, y2, ..., ym)), x1 <= y1
;                                            { y1 union (mergeLists (x1, x2, ..., xn) (y2, y3, ..., ym)), otherwise
(defun mergeLists (xs ys)
    (cond
        ((null xs) ys)
        ((null ys) xs)
        ((<= (first xs) (first ys))
            (cons (first xs) (mergeLists (cdr xs) ys)))
        (t
            (cons (first ys) (mergeLists xs (cdr ys))))))

(format t "Merged lists: ~d ~%" (mergeLists (list 1 4 6) (list 2 3 5)))


; d)
; gcdTwo a b = { a, b = 0
;              { gcdTwo b (a%b), otherwise
(defun gcdTwo (a b)
    (cond
        ((equal b 0) a)
        (t (gcdTwo b (mod a b)))))

; gcdList (x1, x2, ..., xn) = { x1, n = 1
;                             { gcdTwo x1 (gcdList (x2, x3, ..., xn)), otherwise
(defun gcdList (xs)
    (cond
        ((null (cdr xs)) (first xs))
        (t (gcdTwo (first xs) (gcdList (cdr xs))))))

(format t "GCD(22, 121) = ~d ~%" (gcdTwo 22 121))
(format t "GCD([22, 121, 44, 55]) = ~d ~%" (gcdList (list 22 121 44 55)))
(format t "GCD([1024, 512, 100]) = ~d ~%" (gcdList (list 1024 512 100)))

; c)
; notation: list (x1, x2, ..., xn) represents integer (xn ... x3 x2 x1)
; listSum (x1, x2, ..., xn) (y1, y2, ..., yn) carry = { carry, n = 0
;                                                     { ((x1+y1)%10) union (listSum (x2, x3, ..., xn) (y2, y3, ..., yn) ((x1+y1)/10)), otherwise

(defun listSum (xs ys carry)
    (cond
        ((null xs)
            (cond
                ((eq carry 1) (cons 1 nil))
                (t '())))
        (t
            (let
                ((newDigit (mod (+ (+ (first xs) (first ys) carry)) 10))
                (newCarry (floor (+ (first xs) (first ys)) 10)))
            (cons newDigit (listSum (cdr xs) (cdr ys) newCarry))))))

; listToNumber (x1, x2, ..., xn) = { 0, n = 0
;                                  { 10 * listToNumber (x2, x3, ..., xn) + x1, otherwise
(defun listToNumber (xs)
    (cond
        ((null xs) 0)
        (t (+ (* 10 (listToNumber (cdr xs))) (first xs)))))

; solveC (x1, x2, ..., xn) (y1, y2, ..., yn) = listToNumber (listSum (xn, ..., x2, x1) (yn, ..., y2, y1) 0)
(defun solveC (xs ys) (listToNumber (listSum (reverse xs) (reverse ys) 0)))


;(format t "listToNumber([1, 2, 0, 3, 4]) = ~d ~%" (listToNumber (list 1 2 0 3 4)))
;(format t "listSum([1, 2, 3, 4], [5, 6, 7, 8]) = ~d ~%" (listSum (list 1 2 3 4) (list 5 6 7 8) 0))
(format t "~%sum([6, 5, 3, 4], [5, 6, 7, 8]) = ~d ~%" (solveC (list 6 5 3 4) (list 8 6 7 8)))
(format t "sum([9 9] [1 0] = ~d ~%" (solveC (list 9 9) (list 1 0)))


; b)

; join (x1, x2, ..., xn) (y1, y2, ..., ym) = { (y1, y2, ..., ym), n = 0
;                                            { x1 union (join (x2, x3, ..., xn) (y1, y2, ..., ym), otherwise

; replaceElement (x1, x2, ..., xn) a b = { empty set, n = 0
;                                        { replaceElement (x1 a b) UNION replaceElement (x2, x3, ... xn) a b, x1 is list
;                                        { b  UNION (replaceElement (x2, ..., xn) a b), x1 = a
;                                        { x1 UNION (replaceElement (x2, ..., xn) a b), x1 != a
(defun join (xs ys)
    (cond
        ((null xs) ys)
        (t (cons (first xs) (join (cdr xs) ys)))))

(defun replaceElement (xs a b)
    (cond
        ((null xs) '())
        ((listp (first xs)) (cons (replaceElement (first xs) a b) (replaceElement (cdr xs) a b)))
        (t
            (cond
                ((equal (first xs) a) (join b (replaceElement (cdr xs) a b)))
                (t (cons (first xs) (replaceElement (cdr xs) a b)))))))

(format t "~%replaceElement([[1 2] 3 [4 [2] 6] 2] 2 [0 0 0] = ~d ~%" (replaceElement (list (list 1 2) 3 (list 4 (list 2) 6) 2) 2 (list 0 0 0)))
