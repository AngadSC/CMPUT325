#| Question 1
The function returns the count of all the non-numeric atoms wihtin a list, including all the ones 
in a nested list.
|#
(defun count0 (L)
    (if ( null L)    
    0
    (if (atom( car L))
        (if (numberp (car L))
            (+ 0 (count0 (cdr L)))
            ( + 1 (count0 (cdr L))))
        ( + (count0 (car L)) (count0 (cdr L))))))

;test cases
(count0 '(a 1 (b 2 )))
(count0 '(a 1 3 4 (3 a j (b 2))))