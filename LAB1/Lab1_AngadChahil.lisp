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
    (sort (subsets0-helper L S) 
          (lambda (a b) (< (length a) (length b)))))

(defun subsets0-helper (L S)
    (cond 
        ((null L) '(()))
        (t ( let* ((rest-subset (subsets0-helper (cdr L) S)))
            (append rest-subset
                (add-to-sub (car L) rest-subset S))))))

(defun add-to-sub (elem subsets max-size)
    (cond 
        ((null subsets) nil )
        ((> (length (cons elem (car subsets))) max-size)
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
    (sort (subset-helper L S '())
        (lambda ( a b) (< (length a) (length b)))))

(defun subset-helper (L S current)
    (cond   
        ((null L) (list current))
        (t (let ((without-first (subset-helper (cdr L) S current)))
            (if (< (length current) S)
                (append without-first
                    (subset-helper (cdr L) S (cons (car L) current)))
                    without-first)))))



#| Question 5, replace E1 in List L with E2, go through each element recursively replacing if E1 
|#
#|
Test cases for substitute-exp:
(substitute-exp 'a 'x '(a b c a)) => (x b c x)
(substitute-exp 'a 'x '(a (b a) c)) => (x (b x) c)
(substitute-exp '(1 2) '(x y) '((1 2) 3 (1 2))) => ((x y) 3 (x y))
(substitute-exp 'old 'new '()) => ()
(substitute-exp 'a 'b '(c d e)) => (c d e)
(substitute-exp 1 99 '(1 (2 1) (3 (1 4)))) => (99 (2 99) (3 (99 4)))
|#
(defun substitute-exp (E1 E2 L)
    (cond 
        ((null L) '())          ; base case empty list 
        ((equal (car L) E1)     ; is first elem =E1 
            (cons E2 (substitute-exp E1 E2 (cdr L))))   ; replace with E2
        ((atom (car L))                                 ; first elem not E1
            (cons (car L) (substitute-exp E1 E2 (cdr L))))  ; leave it in place 
            (t 
                (cons (substitute-exp E1 E2 (car L))        ; recurisng into lis t
                    (substitute-exp E1 E2 (cdr L))))))


#| Question 6, xdelete function, and helper functions from lecture notes
|#
#|
Test cases for xdelete:
(xdelete nil 5) => NIL
(xdelete '(nil 5 nil) 5) => NIL
(xdelete '(nil 5 nil) 3) => (NIL 5 NIL)
(xdelete '((nil 2 nil) 4 (nil 6 nil)) 2) => (NIL 4 (NIL 6 NIL))
(xdelete '((nil 2 nil) 4 ((nil 5 nil) 6 (nil 8 nil))) 4) => ((NIL 2 NIL) 2 ((NIL 5 NIL) 6 (NIL 8 NIL))) 
|#

(defun isEmptyTree (Tr)
    (null Tr))

(defun create_empty_tree ()
    nil)

(defun create_tree ( L N R)
    (cons L (cons N (cons R nil))))

(defun node_value (Tr)
    (car (cdr Tr)))

(defun left_subtree (Tr)
    (car Tr))

(defun right_subtree (Tr)
    (car (cdr (cdr Tr))))

(defun find)_max (Tr)
    (cond 
        ((isEmptyTree Tr) nil)
        ((isEmptyTree (right_subtree Tr)) (node_value Tr))
        (t (find_max (right_subtree Tr))))

(defun delete_max (Tr)
    (cond 
        ((IsEmptyTree Tr) nil)
        ((IsEmptyTree (right_subtree Tr)) (left_subtree Tr))
        (t (create_tree (left_subtree Tr)
            (node_value Tr)
            (delete_max (right_subtree Tr))))))

(defun xdelete (Tr int)
    (cond 
        ((isEmptyTree Tr) nil)
        ((< Int (node_value Tr))
            (create_tree (xdelete (left_subtree Tr) Int)
                (node_value Tr)
                    (right_subtree Tr)))
            
            ((> Int (node_value Tr))
                (create_tree (left_subtree Tr)
                    (node_value Tr)
                    (xdelete (right_subtree Tr) Int)))
            
            ( t
                (cond
                    ((and (isEmptyTree (left_subtree Tr))
                        (isEmptyTree (right_subtree Tr)))
                        nil)
                        
                    ((isEmptyTree (left_subtree Tr))
                        (right_subtree Tr))
                    ((isEmptyTree (right_subtree Tr))
                        (left_subtree Tr))
                        (t
                            (let ((max_left (find_max (left_subtree Tr))))
                                (create_tree (delete_max (left_subtree Tr))
                                    max_left
                                    (right_subtree Tr))))))))


(print "========================================")
(print "TESTING XDELETE")
(print "========================================")
(print "(xdelete nil 5)")
(print (xdelete nil 5))
(print "Expected: NIL")
(print "")

(print "(xdelete '(nil 5 nil) 5)")
(print (xdelete '(nil 5 nil) 5))
(print "Expected: NIL")
(print "")

(print "(xdelete '(nil 5 nil) 3)")
(print (xdelete '(nil 5 nil) 3))
(print "Expected: (NIL 5 NIL)")
(print "")

(print "(xdelete '((nil 2 nil) 4 (nil 6 nil)) 2)")
(print (xdelete '((nil 2 nil) 4 (nil 6 nil)) 2))
(print "Expected: (NIL 4 (NIL 6 NIL))")
(print "")

(print "(xdelete '((nil 2 nil) 4 (nil 6 nil)) 4)")
(print (xdelete '((nil 2 nil) 4 (nil 6 nil)) 4))
(print "Expected: ((NIL 2 NIL) 2 (NIL 6 NIL)) or similar")
(print "")