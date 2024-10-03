#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2024
;;
;; Lab #7
;;
;; Annalee Nersten
;; 
;;
;; This program evaluates expressions including if, cond, let, lambda,
;; and letrec statements  
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide lookup 
         evaluate
         special-form?
         evaluate-special-form)

(define lookup
    (lambda (symbol environment)
        (cond ((equal? (symbol? (car(car environment))) #f) (error "Error. Argument is not a symbol"))
              ((equal? symbol (car(car environment))) (car(cdr(car environment))))
              ((null? (cdr environment)) (error "Error. Symbol does not exist"))
              (else (lookup symbol (cdr environment))))))

(define special-form?
  (lambda (expression)
    (cond ((and (list? expression) (equal? (or (equal? 'if (car expression)) (equal? 'cond (car expression)))#t))#t)
          ((and (list? expression) (equal? 'let (car expression))) #t)
          ((and (list? expression) (equal? 'lambda (car expression))) #t)
          ((and (list? expression) (equal? 'letrec (car expression))) #t)
          (else #f))))

(define evalCond
  (lambda (expression environment)
    (cond ((equal? (evaluate (car(car expression)) environment) #t) (evaluate (car(cdr(car expression))) environment))
          ((equal? (car(car expression)) 'else) (evaluate (cdr(car expression)) environment))
          (else (evalCond (cdr expression) environment)))))

(define evalLet
  (lambda (expression newEnvironment environment newList)
    (cond ((null? expression) newEnvironment)
          (else 
          (evalLet (cdr expression) (append (cons (list (car(car expression)) (evaluate (cadar expression) environment)) newList) newEnvironment) environment newList)))))

(define evalLetrec
  (lambda (expression environment)
      (let ((MiniEnvironment (miniEnvironment (car expression) environment '())))
        (let ((newEnvironment (append MiniEnvironment environment)))
          (fix-closures MiniEnvironment newEnvironment)
          (evaluate (car(cdr expression)) newEnvironment)))))

(define miniEnvironment 
  (lambda (expression OldEnvironment MiniEnvironment)
    (cond ((null? expression) MiniEnvironment)
          (else
            (miniEnvironment (cdr expression) OldEnvironment
                             (cons (list (car(car expression)) (evaluate  (cadar expression) OldEnvironment)) MiniEnvironment))))))

(define fix-closures
  (lambda (MiniEnvironment NewEnvironment)
    (cond ((null? MiniEnvironment) NewEnvironment)
          ((closure? (car(cdr(car MiniEnvironment))))
           (set-closure-env! (car(cdr(car MiniEnvironment))) NewEnvironment)
           (fix-closures (cdr MiniEnvironment) NewEnvironment))
          (else
           (fix-closures (cdr MiniEnvironment) NewEnvironment)))))

(define closure
(lambda (vars body env)
(mcons 'closure (mcons env (mcons vars body)))))
(define closure?
(lambda (clos) (and (mpair? clos) (eq? (mcar clos) 'closure))))
(define closure-env
(lambda (clos) (mcar (mcdr clos))))
(define closure-vars
(lambda (clos) (mcar (mcdr (mcdr clos)))))
(define closure-body
(lambda (clos) (mcdr (mcdr (mcdr clos)))))
(define set-closure-env!
(lambda (clos new-env) (set-mcar! (mcdr clos) new-env)))

(define apply-closure
  (lambda (closure values)
    (evaluate (closure-body closure) (append (map list (closure-vars closure) values) (closure-env closure)))))

(define evaluate-special-form
  (lambda (expression environment)
    (cond ((and (equal? 'if (car expression)) (equal? (evaluate (car(cdr expression)) environment) #t)) (evaluate (car(cdr(cdr expression))) environment))
          ((and (equal? 'if (car expression)) (equal? (evaluate (car(cdr expression)) environment) #f)) (evaluate (car(cdr(cdr(cdr expression)))) environment))
          ((and (equal? 'cond (car expression)) (equal? (evaluate (car(car(cdr expression))) environment) #t)) (evaluate (car(cdr(car(cdr expression)))) environment))
          ((equal? 'let (car expression)) (evaluate (caddr expression) (evalLet (car(cdr expression)) environment environment '())))
          ((equal? 'lambda (car expression)) (closure (cadr expression) (caddr expression) environment))
          ((equal? 'letrec (car expression)) (evalLetrec (cdr expression) environment))
          (else (evalCond (cdr expression) environment)))))

(define apply-function
  (lambda (evalCar evalCdr)
    (cond ((procedure? evalCar) (apply evalCar evalCdr))
          ((closure? evalCar) (apply-closure evalCar evalCdr))
          (else (error "unknown function type")))))

(define evaluate
  (lambda (expression environment)
    (cond ((special-form? expression) (evaluate-special-form expression environment))
          ((number? expression) expression)
          ((symbol? expression) (lookup expression environment))
          ((list? expression)
           (let ((evalElements (map (lambda (element)(evaluate element environment)) expression)))
             (apply-function (car evalElements) (cdr evalElements))))
          (else (error "Not a valid expression")))))