

; helpers fucnctuions 
;checks that check if sum is a primitive function or not 
(defun fl-primop-p (sym)
    (and (symbolp sym)
        (member sym             ; just checks if its a part of one of those primrtive operaitons 
            '((if cond and or quote             
                 null atom eq equal numberp      
                 car cdr first rest cons list   
                 + - * / < > = <= >= abs not)     
               :test #'eq))))

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
                (if (and (eq name fname)
                    (listp params)
                    (eq eqsign '=)
                    (= (length params) arg))
                    d
                    (fl-lookup-def fname arg (cdr p)))))))


