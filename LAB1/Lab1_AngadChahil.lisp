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

(defun find_max (Tr)
    (cond 
        ((isEmptyTree Tr) nil)
        ((isEmptyTree (right_subtree Tr)) (node_value Tr))
        (t (find_max (right_subtree Tr)))))

(defun delete_max (Tr)
    (cond 
        ((isEmptyTree Tr) nil)
        ((isEmptyTree (right_subtree Tr)) (left_subtree Tr))
        (t (create_tree (left_subtree Tr)
            (node_value Tr)
            (delete_max (right_subtree Tr))))))

(defun xdelete (Tr int)
    (cond 
        ((isEmptyTree Tr) nil)
        ((< int (node_value Tr))
            (create_tree (xdelete (left_subtree Tr) int)
                (node_value Tr)
                    (right_subtree Tr)))
            
            ((> int (node_value Tr))
                (create_tree (left_subtree Tr)
                    (node_value Tr)
                    (xdelete (right_subtree Tr) int)))
            
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




#| Question 7 (A,B) page A links to B, given list of pairs, search through list
to find the pages, retrun list of pages that can be reacehed by X
SO find all pairs that are X,Y - > and return a list of the Ys. 
|#

(defun reached (x L)
    (remove-dups (reached-help (list x) L '())))

; helper functin that uses accumulator for the pages that have been visited
(defun reached-help (to-visit L visited)
    (cond 
        ((null to-visit) visited) 
        (t 
            (let ((current (car to-visit)))
                (if (is-member current visited)
                (reached-help (cdr to-visit) L visited)
                (reached-help
                
                    (append (cdr to-visit) (find-neighbours current L))
                    L  
                    (cons current visited)))))))

; find all pages that the page we are on connect to 
(defun find-neighbours (page L)
    (cond 
        ((null L) '())
        ((equal (car ( car L)) page)
            (cons (cadr (car L)) (find-neighbours page (cdr L))))
        (t (find-neighbours page(cdr L)))))

; check if is-member
(defun is-member (item L)
    (cond   
    ((null L) nil)
    ((equal item (car L)) t)
    (t (is-member item (cdr L)))))

;remove duplicates 
(defun remove-dups (L)
    (cond   
    ((null L) '())
    ((is-member (car L) (cdr L)) (remove-dups (cdr L)))
    (t (cons (car L) (remove-dups (cdr L))))))

(defun rank (S L)
    (let ((counts (count-ref S L)))
        (extract-page (my-sort counts))))

;count refs for each page 
(defun count-ref (S L)
    (cond 
        ((null S) '())
        (t (cons (list (car S) (count-ref-to (car S) L))
        (count-ref (cdr S) L)))))

;count the number of page refences to a target page 
(defun count-ref-to (target-page L)
    (my-length (remove-dups
    (filter-ref target-page L))))

(defun my-length (L)
    (cond 
    ((null L) 0)
    (t (+ 1 (my-length (cdr L))))))

;list of pages that refernce te page we want 
(defun filter-ref (target-page L)
    (cond 
        ((null L) '())
        ((and (equal (cadr (car L)) target-page)
            (not (equal (car ( car L)) target-page)))
            (cons (car (car L)) (filter-ref target-page (cdr L))))
            (t (filter-ref target-page (cdr L)))))

(defun extract-page (L)
    (cond 
        ((null L) '())
        (t (cons (car (car L))  (extract-page (cdr L))))))

#| Sorting function uses a simple insertion sort 
|#
(defun my-sort (L)
    (cond 
    ((null L) '())
    (t (insert-sorted (car L) (my-sort (cdr L))))))

(defun insert-sorted (elem sorted-list) 
    (cond 
        ((null sorted-list) (list elem))
        ((>= (cadr elem) (cadr (car sorted-list)))
            (cons elem sorted-list))
            (t (cons (car sorted-list) (insert-sorted elem (cdr sorted-list))))))

(print "========================================")
(print "TESTING REACHED")
(print "========================================")
(print "(reached 'A '((A B) (B C) (A D) (D E)))")
(print (reached 'A '((A B) (B C) (A D) (D E))))
(print "Expected: (A B C D E) in some order")
(print "")

(print "(reached 'A '((A B) (C D)))")
(print (reached 'A '((A B) (C D))))
(print "Expected: (A B)")
(print "")

(print "(reached 'X '((A B) (B C)))")
(print (reached 'X '((A B) (B C))))
(print "Expected: (X)")
(print "")

#|
Test cases for rank:
|#

(print "========================================")
(print "TESTING RANK")
(print "========================================")
(print "(rank '(A B C) '((B A) (C A) (B C)))")
(print (rank '(A B C) '((B A) (C A) (B C))))
(print "Expected: (A C B)")
(print "")

(print "(rank '(Google Facebook Twitter) '((Google Facebook) (Google Twitter) (Facebook Google)))")
(print (rank '(Google Facebook Twitter) '((Google Facebook) (Google Twitter) (Facebook Google))))
(print "Expected: Google and Facebook tied (2 refs), Twitter (1 ref)")
(print "")

(print "(rank '(Home About Contact) '((Home About) (Home Contact) (About Home) (About Contact)))")
(print (rank '(Home About Contact) '((Home About) (Home Contact) (About Home) (About Contact))))
(print "Expected: Home and Contact (2 refs each), About (1 ref)")
(print "")

