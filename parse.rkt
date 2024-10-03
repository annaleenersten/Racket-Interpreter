#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2024
;;
;; Lab #8
;;
;; Annalee Nersten
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide parse)

(define char-symbolic?
  (lambda (char) (and (not (char-whitespace? char))
                      (not (D? char))
                      (not (eq? char #\())
                      (not (eq? char #\))))))

(define D? char-numeric?)

(define A? char-symbolic?)

(define E?
  (lambda (char) (or  (D? char)
                      (eq? char #\()
                      (char-symbolic? char))))

(define char->number
  (lambda (char)
    (- (char->integer char)
       (char->integer #\0))))

(define parse
  (lambda (str)
    (first (L (string->list str)))))

(define L
  (lambda (input)
    (cond
     [(null? input) (cons null null)]
     
     [(char-whitespace? (first input)) (L (rest input))]

     [(eq? (first input) #\)) (cons '() (rest input))]
     
      [(E? (first input)) (let* ((e-rest (E input))
                                (l-rest (L (rest e-rest))))
                           
                           (cons (cons (first e-rest)
                                       (first l-rest))
                                 (rest l-rest)))]
      [else (error "error")])))

(define E
  (lambda(input)
    (cond
    
    [(eq? (first input) #\() (L (rest input))]

    [(D? (first input)) (let* ((d-rest (D input))
                                (n-rest (N (rest d-rest) (first d-rest)))) n-rest)]
                           
    [(A? (first input)) (let* ((a-rest (A input))
                                (s-rest (S (rest a-rest) (first a-rest)))) s-rest)]

    [else (error "error")])))
    
(define S 
  (lambda (input num)
     (cond
     [(or (null? input)
          (not (char-symbolic? (first input)))) (cons num input)]

     [else (let* ((a-rest (A input))
                  (s-rest (S (rest a-rest)
                                (string->symbol(string-append (symbol->string num) (symbol->string(first a-rest)))))
                             ))
             s-rest)])))


(define N
  (lambda (input num)
    (cond
     [(or (null? input)
          (not (D? (first input)))) (cons num input)]

     [else (let* ((d-rest (D input))
                  (n-rest (N (rest d-rest)
                             (+ (* num 10)
                                (first d-rest))
                             )))
             n-rest)])))

(define D
  (lambda (input)
    (cond
     [(null? input) (error (string-append "Not a digit:"
                                          (list->string input)))]
     
     [(D? (first input)) (cons (char->number (first input))
                               (rest input))]
           
      [else (error (string-append "Not a digit:"
                                  (list->string input)))])))

(define A
  (lambda (input)
    (cond
     [(null? input) (error (string-append "Not symbolic:"
                                          (list->string input)))]
     

     [(char-symbolic? (first input)) (cons (string->symbol(string (first input)))
                               (rest input))]
           
      [else (error (string-append "Not symbolic:"
                                  (list->string input)))])))