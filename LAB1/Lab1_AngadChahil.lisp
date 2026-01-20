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
    ((equal elem (car list)) t) 
    (t ( is_equal elem (cdr list)))))

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
(print (rm-duplicate '( a b c)))
(print (rm-duplicate '(a b a c )))
(print (rm-duplicate '(1 2 3 2 1)))
(print (rm-duplicate '((1 2 ) (3 4) (1 2))))
(print (rm-duplicate'()))


#| Question 3 part a , alternates between list L1 and L2, returning a list that has all the items 
|#

(defun mix (L1 L2)
    cond 
        ((null L1) L2)
        (( null L2) L1)
        (t (cons (car L1)
                (mix L2 (cdr L1)))))


#| Question 3 part b, returns two lists, one with all odd element positons 
one with even element positions
|#

(defun split (L) 
    (cond 
        ((null L) '(() ()))
        ((null ( cdr L)) ( list ( list ( car L)) '()))
        (t (let ((rest (split (cddr L))))
            (list (cons (car L) (car rest))
                (cons (cadr L) (cadr rest)))))))





#| Question 4 , non accumalator solution retrunign subsets of size S, inputting a list L 
Build the result after recursion is completed 
|#
(defun subsets0 (L S)
    (cond 
        ((equal S 0) '(()))     ; if S is 0, only subset is empty list
        ((null L ) '(())        ; if List is emtpy 
        

        (t
            (let* ((first-elem (car L))     ;gets first elem 
                    (rest-list) (cdr L))     ; rest of list 
                    (rest-subset (subsets0 rest-list S)))   ;gets the subsets with first elem missing 
                    (append rest-subset 
                    (add-to-sub first-elem rest-subset(- S 1)))))))     ; takes subsets that dont have first elem, and add first elem 
(defun add-to-sub (elem subsets max-size)
(cond 
    ((null subsets) nil)
    ((> (length ( car subsets)) maz-size)
        (add-to-sub elem (cdr subsets) max-size))
        
        (t 
        (cons (cons elem (car subsets))
                (add-to-sub elem (cdr subsets) max-size)))))

