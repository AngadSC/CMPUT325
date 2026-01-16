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
;(print(count0 '(a 1 (b 2 ))))
;(print(count0 '(a 1 3 4 (3 a j (b 2)))))

#| Question 2 
The function returns a list that removes all duplicate elements
3 functions, one to check if element exists in a list 
one to remove duplicate and one that calls it
|#
;helper function: check if an element in a list 
(defun is_equal (elem list)
    (cond
    ((null list) nil) 
    (equal elem (car list)) t) 
    (t ( is_equal elem (cdr list))))

;removes the duplicates
(defun rm_dup_helper (x seen)
    (cond 
    ((null x) nil) 
    ((is_equal (car x) seen)
    (rm_dup_helper (cdr x) seen))
    (t ( cons ( car x)
    (rm_dup_helper (cdr x)
            (cons(car x) seen))))))


;main function
(defun rm-duplicate (x)
(rm_dup_helper x nil)) 



;test cases
(rm-duplicate '( a b c))
(rm-duplicate '(a b a c ))
(rm-duplocate '(1 2 3 2 1))
(rm-duplicate '((1 2 ) (3 4) (1 2)))
(rm-duplicate'())