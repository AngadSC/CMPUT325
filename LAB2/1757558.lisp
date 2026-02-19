

; helpers fucnctuions 
;checks that check if sum is a primitive function or not 
(defun fl-primop-p (sym)
    (and (symbolp sym)
        (member sym             ; just checks if its a part of one of those primrtive operaitons 
            '(if cond and or quote
                 null atom eq equal numberp
                 car cdr first rest cons list
                 + - * / < > = <= >= abs not)
               :test #'eq)))
; check if e is a lambdaexpression with the form (lambda (params) body)

(defun fl-lambda-p (e)
    (and (consp e ) 
        (eq (car e) 'lambda)               ; checks if first is lambda        
       (consp (cdr e))                     ; checks if there is a second elelent     
       (consp (cddr e))                       ; checks fort the thuird     
       (null (cdddr e))))                   ; makes sure there isnt a 4th 


; function fro looking up what function is being called 
; so we take the first definciton d from the p liust nad extract the function named if d is list
; we get param liust, and we get the third elelent which we expect to be = 
; and we get the 4th eleentn and check if the name matcehs the reqruested fname 
; abd we run checks to make sure evberything is as we expected, and only pass the defintion if all of these have been passed 
(defun fl-lookup-def (fname arg p)
    (cond 
        ((null p) nil) 
        (t  
            (let* ((d (car p))
                (name (and (consp d) (car d)))
                (params (and (consp d) (cadr d)))
                (eqsign (and ( consp d) (caddr d)))
                (body ( and (consp d) (cadddr d))))
                (declare (ignore body))
                (if (and (eq name fname)
                    (listp params)
                    (eq eqsign '=)
                    (= (length params) arg))
                    d
                    (fl-lookup-def fname arg (cdr p)))))))


(defun fl-user-fun-p (sym arg p)            
  (and (symbolp sym)                            
       (fl-lookup-def sym arg p))) 

;go through the pairs until we find a paiur that = the key, and recurse til we do 
; so given a var we find the value it algins to 
(defun fl-assoc (key alist)
  (cond
    ((null alist) nil)
    ((eq (caar alist) key) (car alist))
    (t (fl-assoc key (cdr alist)))))


; this is for making suyre when we use a lambda function we dont use vars from outside and stay with the ones we defined 

(defun fl-remove-keys (keys alist)
  (cond
    ((null alist) nil)
    ((member (caar alist) keys :test #'eq)
     (fl-remove-keys keys (cdr alist)))
    (t (cons (car alist)
             (fl-remove-keys keys (cdr alist))))))


; subsitrutes in symbols 
(defun fl-subst (expr env)                      
  
  (cond
   
    ((atom expr)                                ; if expression is an atom 
     (cond
       ((or (eq expr t) (null expr)) expr)         ; keeps the bools the csame 
       ((numberp expr) expr)                        ; if its s asymbol we might need to replace 
       ((symbolp expr)                             
        (let ((pair (fl-assoc expr env)))          
          (if pair (cdr pair) expr)))             ; reoklace with value, else keep symbol as is 
       (t expr)))                                 

    
    ((and (consp expr) (eq (car expr) 'quote))     
     expr)                                        

   
    ((fl-lambda-p expr)                           ; checks if exprssions is lambda 
     (let* ((params (cadr expr))                  
            (body (caddr expr))                   ; gets teh param kust and body 
            (env2 (fl-remove-keys params env)))   
       (list 'lambda params                        ; remakes thge lamda expresions 
             (fl-subst body env2))))             

    
    (t (mapcar (lambda (x) (fl-subst x env)) expr)))) 

(defun fl-pairlis (keys vals)
  (cond 
    ((null keys) nil)
    (t
      (cons (cons(car keys) (car vals))
        (fl-pairlis (cdr keys) (cdr vals))))))

;evalutes an argyuement expression 
(defun fl-eval-args (a p)
    (cond 
    ((null a) nil)                  ; iuf we dont have aby retyurns empty lsit 
        (t (cons (fl-interp (car a) p)      ; eautes the fitst arguement adn recursees 
            (fl-eval-args (cdr a) p)))))


; applying the primtives 


(defun fl-apply-prim (op args p)
  (cond
    ((eq op 'quote)
     (car args))

    ((eq op 'if)
     (let ((c (fl-interp (first args) p)))
       (if c
           (fl-interp (second args) p)
           (fl-interp (third args) p))))

    ((eq op 'cond)
     (labels ((eval-clauses (cls)
                (cond
                  ((null cls) nil)
                  (t
                   (let* ((cl   (car cls))
                          (test (car cl))
                          (expr (cadr cl)))
                     (if (fl-interp test p)
                         (fl-interp expr p)
                         (eval-clauses (cdr cls))))))))
       (eval-clauses args)))

    ;; AND short-circuit
    ((eq op 'and)
 (labels ((sc (xs)
               (cond
                 ((null xs) t)
                 (t
                  (let ((v (fl-interp (car xs) p)))
                    (if v (sc (cdr xs)) nil))))))
   (sc args)))

((eq op 'or)
 (labels ((sc (xs)
               (cond
                 ((null xs) nil)
                 (t
                  (let ((v (fl-interp (car xs) p)))
                    (if v v (sc (cdr xs))))))))
   (sc args)))

    ;; eager primitives
    (t
     (let ((ev (fl-eval-args args p)))
       (cond
         ((eq op 'null)    (null (first ev)))
         ((eq op 'atom)    (atom (first ev)))
         ((eq op 'numberp) (numberp (first ev)))
         ((eq op 'eq)      (eq (first ev) (second ev)))
         ((eq op 'equal)   (equal (first ev) (second ev)))

         ((or (eq op 'car) (eq op 'first)) (car (first ev)))
         ((or (eq op 'cdr) (eq op 'rest))  (cdr (first ev)))
         ((eq op 'cons) (cons (first ev) (second ev)))
         ((eq op 'list) ev)

         ((eq op '+)  (+  (first ev) (second ev)))
         ((eq op '-)  (-  (first ev) (second ev)))
         ((eq op '*)  (*  (first ev) (second ev)))
         ((eq op '/)  (/  (first ev) (second ev)))
         ((eq op '<)  (<  (first ev) (second ev)))
         ((eq op '>)  (>  (first ev) (second ev)))
         ((eq op '=)  (=  (first ev) (second ev)))
         ((eq op '<=) (<= (first ev) (second ev)))
         ((eq op '>=) (>= (first ev) (second ev)))
         ((eq op 'abs) (abs (first ev)))
         ((eq op 'not) (not (first ev)))

         (t (cons op args)))))))

;lambda applicaiton 
; applies the lamdba expression  LAM 
; extracts param list from the body 
; 
;
(defun fl-apply-lambda (lam arg-exprs p) 
    (let* ((params (cadr lam))
            (body  (caddr lam))
            (evargs (fl-eval-args arg-exprs p))
            (env (fl-pairlis params evargs))
            (body2 (fl-subst body env)))
      (fl-interp body2 p)))

(defun fl-proper-list-p (x)
  (cond 
    ((null x ) t)
    ((consp x) (fl-proper-list-p (cdr x)))
    (t nil)))
; main inteprerter 

(defun fl-interp (E P)
(cond
; if its an atom itself 
    ((atom E) E)
    ((and (consp E) (not (fl-proper-list-p E))) E)
    
    (t
    (let* ((f(car E))
        (args (cdr E)))
      ; if operato is primtive then we recongize it an apploy the primaitve operatio nit needs by helper funciton aboe   
      (cond 
        ((and (symbolp f) (fl-primop-p f))
        (fl-apply-prim f args P))

     ; for a defined ufnciton , substitues the function into the body as needed 
            ((and (symbolp f) (fl-user-fun-p f (length args) P)) 
          (let* ((def (fl-lookup-def f (length args) P))      
                 (params (cadr def))                           
                 (body (cadddr def))                          
                 (evargs (fl-eval-args args P))                
                 (env (fl-pairlis params evargs))              
                 (body2 (fl-subst body env)))                
            (fl-interp body2 P)))    
; direct application for lambda 
             ((fl-lambda-p f)  
             (fl-apply-lambda f args P))  
             ((consp f)                                   
          (let ((fv (fl-interp f P)))                
            (cond
              ((fl-lambda-p fv)                         
               (fl-apply-lambda fv args P))           
              ((symbolp fv)                            
               (fl-interp (cons fv args) P))           
              (t E))))                                    

       ; undefined operator 
         (t E))))))


;from notes 
(defparameter *user-defined-progs*
  '(
    (double (x) = (+ x x))
    (quadruple (x) = (double (double x)))
    (my-len (L) = (if (null L) 0 (+ 1 (my-len (cdr L)))))
    (my-member (x L) = (if (null L) nil (if (equal x (car L)) T (my-member x (cdr L)))))
    (my-append (L1 L2) = (if (null L1) L2 (cons (car L1) (my-append (cdr L1) L2))))
    (my-reverse (L) = (if (null L) nil (my-append (my-reverse (cdr L)) (cons (car L) nil))))
    (fib (n) = (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
    (range (n) = (if (= n 0) nil (cons n (range (- n 1)))))
    (my-map (F L) = (if (null L) nil (cons (F (car L)) (my-map F (cdr L)))))
    (my-filter (P L) = (if (null L) nil (if (P (car L)) (cons (car L) (my-filter P (cdr L))) (my-filter P (cdr L)))))
    (my-reduce (F Acc L) = (if (null L) Acc (my-reduce F (F Acc (car L)) (cdr L))))))
