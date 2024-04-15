(define a  2 )
(define b  2 )


(define sum  (lambda y (+ 1 y)) )


(define get_last (lambda x (if (null (cdr x) )  (car x) (get_last (cdr x))   )) )

(get_last ( list 1 2 3 4 5) )

(+ 1 2)
