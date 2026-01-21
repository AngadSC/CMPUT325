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


#|
;test cases
(print (rm-duplicate '( a b c)))
(print (rm-duplicate '(a b a c )))
(print (rm-duplicate '(1 2 3 2 1)))
(print (rm-duplicate '((1 2 ) (3 4) (1 2))))
(print (rm-duplicate'()))
|#

#| Question 3 part a , alternates between list L1 and L2, returning a list that has all the items 
|#
#|
Test cases for mix:
(mix '(a c e) '(b d f)) => (a b c d e f)
(mix '(a c e f) '(b d)) => (a b c d e f)
(mix '(a c) '(b d e f)) => (a b c d e f)
(mix '() '(1 2 3)) => (1 2 3)
(mix '(x y z) '()) => (x y z)
|#

(defun mix (L1 L2)
    (cond 
        ((null L1) L2)
        (( null L2) L1)
        (t (cons (car L1)
                (mix L2 (cdr L1))))))


#| Question 3 part b, returns two lists, one with all odd element positons 
one with even element positions
|#
#|
Test cases for split:
(split '(a b c d e f)) => ((a c e) (b d f))
(split '(1 2 3 4 5)) => ((1 3 5) (2 4))
(split '(x y)) => ((x) (y))
(split '(z)) => ((z) ())
(split '()) => (() ())
(split '(a b c d)) => ((a c) (b d))
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


#|
Test cases for subsets0:
(subsets0 '(a b c) 2) => (() (a) (b) (c) (a b) (a c) (b c))
(subsets0 '(1 2) 1) => (() (1) (2))
(subsets0 '(x) 0) => (())
(subsets0 '(a b c) 3) => (() (a) (b) (c) (a b) (a c) (b c) (a b c))
(subsets0 '() 5) => (())
|#

(defun subsets0 (L S)
    (cond 
        ((equal S 0) '(()))     ; if S is 0, only subset is empty list
        ((null L ) '(())))      ; if List is emtpy 
        

           (t (let* ((first-elem (car L))  ;gets first elem 
                        (rest-list (cdr L))      ; rest of list 
                        (rest-subset (subsets0 rest-list S)))   ;gets the subsets with first elem missing 
                    (append rest-subset 
                         (add-to-sub first-elem rest-subset(- S 1))))))     ; takes subsets that dont have first elem, and add first elem 
(defun add-to-sub (elem subsets max-size)
(cond 
    ((null subsets) nil)
    ((> (length ( car subsets)) max-size)
        (add-to-sub elem (cdr subsets) max-size))
        
        (t 
        (cons (cons elem (car subsets))
                (add-to-sub elem (cdr subsets) max-size)))))


#| Question 4 with an accumulator 
|#
#|
Test cases for subsets (with accumulator):
(subsets '(a b c) 2) => (() (a) (b) (c) (a b) (a c) (b c))
(subsets '(a b c) 1) => (() (a) (b) (c))
(subsets '(1 2) 1) => (() (1) (2))
(subsets '(x) 0) => (())
(subsets '(a b c) 3) => (() (a) (b) (c) (a b) (a c) (b c) (a b c))
(subsets '() 5) => (())
|#

(defun subsets (L S)
    (subset-help L S '() '(())))       ; empty set for acumulation 

(defun subset-help (L S current accumulate )
    (cond 
        ((null L) accumulate) 
        (t ( let ((first(if ( < (length current) S)
                (cons (car L) current)
                nil)))
                (subset-help (cdr L) S current
                (if first
                (subset-help (cdr L) S first accumulate)
                accumulate))))))



(print "========================================")
(print "TESTING MIX")
(print "========================================")
(print "(mix '(a c e) '(b d f))")
(print (mix '(a c e) '(b d f)))
(print "Expected: (A B C D E F)")
(print "")

(print "(mix '(a c e f) '(b d))")
(print (mix '(a c e f) '(b d)))
(print "Expected: (A B C D E F)")
(print "")

(print "(mix '(a c) '(b d e f))")
(print (mix '(a c) '(b d e f)))
(print "Expected: (A B C D E F)")
(print "")

(print "(mix '() '(1 2 3))")
(print (mix '() '(1 2 3)))
(print "Expected: (1 2 3)")
(print "")

(print "(mix '(x y z) '())")
(print (mix '(x y z) '()))
(print "Expected: (X Y Z)")
(print "")

(print "========================================")
(print "TESTING SPLIT")
(print "========================================")
(print "(split '(a b c d e f))")
(print (split '(a b c d e f)))
(print "Expected: ((A C E) (B D F))")
(print "")

(print "(split '(1 2 3 4 5))")
(print (split '(1 2 3 4 5)))
(print "Expected: ((1 3 5) (2 4))")
(print "")

(print "(split '(x y))")
(print (split '(x y)))
(print "Expected: ((X) (Y))")
(print "")

(print "(split '(z))")
(print (split '(z)))
(print "Expected: ((Z) NIL)")
(print "")

(print "(split '())")
(print (split '()))
(print "Expected: (NIL NIL)")
(print "")

(print "(split '(a b c d))")
(print (split '(a b c d)))
(print "Expected: ((A C) (B D))")
(print "")

(print "========================================")
(print "TESTING SUBSETS0 (without accumulator)")
(print "========================================")
(print "(subsets0 '(a b c) 2)")
(print (subsets0 '(a b c) 2))
(print "Expected: (NIL (A) (B) (C) (A B) (A C) (B C))")
(print "")

(print "(subsets0 '(a b c) 1)")
(print (subsets0 '(a b c) 1))
(print "Expected: (NIL (A) (B) (C))")
(print "")

(print "(subsets0 '(1 2) 1)")
(print (subsets0 '(1 2) 1))
(print "Expected: (NIL (1) (2))")
(print "")

(print "(subsets0 '(x) 0)")
(print (subsets0 '(x) 0))
(print "Expected: (NIL)")
(print "")

(print "(subsets0 '(a b c) 3)")
(print (subsets0 '(a b c) 3))
(print "Expected: (NIL (A) (B) (C) (A B) (A C) (B C) (A B C))")
(print "")

(print "(subsets0 '() 5)")
(print (subsets0 '() 5))
(print "Expected: (NIL)")
(print "")

(print "========================================")
(print "TESTING SUBSETS (with accumulator)")
(print "========================================")
(print "(subsets '(a b c) 2)")
(print (subsets '(a b c) 2))
(print "Expected: (NIL (C) (B) (B C) (A) (A C) (A B))")
(print "")

(print "(subsets '(a b c) 1)")
(print (subsets '(a b c) 1))
(print "Expected: (NIL (C) (B) (A))")
(print "")

(print "(subsets '(1 2) 1)")
(print (subsets '(1 2) 1))
(print "Expected: (NIL (2) (1))")
(print "")

(print "(subsets '(x) 0)")
(print (subsets '(x) 0))
(print "Expected: (NIL)")
(print "")

(print "(subsets '(a b c) 3)")
(print (subsets '(a b c) 3))
(print "Expected: (NIL (C) (B) (B C) (A) (A C) (A B) (A B C))")
(print "")

(print "(subsets '() 5)")
(print (subsets '() 5))
(print "Expected: (NIL)")
(print "")

(print "========================================")
(print "ALL TESTS COMPLETE")
(print "========================================")