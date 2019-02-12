;; This file contains 19 versions of the factorial
;; function. Have fun!
;;
;; Author:   Tord Svensson

;; 1. 
;; The regular factorial procedure
(define (fac n)
  (if (= n 0) 1
      (* n (fac (- n 1)))))


;; 2. 
;; The regular factorial procedure with
;; reversed multiplication order
(define (facr n)
  (define (loop r)
    (if (= r n) r
        (* r (loop (+ r 1)))))
  (loop 1))

;; 3. 
;; The iterative version of the factorial
;; procedure.
(define (faci n)
  (define (_faci n r)
    (if (= n 0) r
        (_faci (- n 1) (* n r))))
  (_faci n 1))

;; 4. 
;; The iterative version of the factorial
;; procedure with reversed multiplication order.
(define (facir n)
  (define (loop m r) 
    (if (> m n) r
        (loop (+ m 1) (* m r))))
  (loop 1 1))

;; 5. 
;; A continuation based version of the
;; factorial procedure.
(define (facc n cont)
  (if (= n 0) (cont 1)
      (facc (- n 1) 
             (lambda (r) (cont (* n r))))))


;; 6. 
;; A continuation based version of the
;; factorial procedure with reversed
;; multiplication order.
(define (faccr n cont)
  (define (_facc m cont)
    (if (= m n) (cont m)
        (_facc (+ m 1) 
               (lambda (r) (cont (* m r))))))
  (_facc 1 cont))


;; 7. 
;; A recursive version of the factorial procedure
;; with anonymous recursion. 
(define facl (lambda (n)
               ((lambda (f)
                  (f f n))
                (lambda (f n)
                  (if (= n 0) 1
                      (* n (f f (- n 1))))))))
                 

;; 8. 
;; A recursive version of the factorial procedure
;; with anonymous recursion and reversed multiplication
;; order.
(define faclr (lambda (n)
               ((lambda (f)
                  (f f 1))
                (lambda (f m)
                  (if (= m n) m
                      (* m (f f (+ m 1))))))))
             
;; 9. 
;; A iterative version of the factorial procedure
;; with anonymous recursion. 
(define facli (lambda (n)
                ((lambda (f)
                   (f f n 1))
                 (lambda (f n r)
                   (if (= n 0) r
                       (f f (- n 1) (* n r)))))))


;; 10. 
;; A iterative version of the factorial procedure
;; with anonymous recursion and reversed multiplication
;; order.
(define faclir (lambda (n)
                ((lambda (f)
                   (f f 1 1))
                 (lambda (f m r)
                   (if (> m n) r
                       (f f (+ m 1) (* m r)))))))


;; 11. 
;; A continuation based version of the factorial procedure
;; with anonymous recursion 
(define faclc 
  (lambda (n)
    ((lambda (f)
       (f f n id))
     (lambda (f n c)
       (if (= n 0) (c 1)
           (f f (- n 1) 
              (lambda (v) 
                (c (* n v)))))))))


;; 12. 
;; A continuation based version of the factorial procedure
;; with anonymous recursion and reversed multiplication
;; order.
(define faclcr 
  (lambda (n)
    ((lambda (f)
       (f f 1 id))
     (lambda (f m c)
       (if (= m n) (c m)
           (f f (+ m 1) 
              (lambda (v) 
                (c (* m v)))))))))

                        

(define (id x) x)


;; 13. 
;; An iterative version of the factorial procedure using do
;; and set!.
(define (facdo n)
  (do ((r 1)
       (i n (- i 1)))
       ((= 0 i) r)
    (set! r (* i r))))



;; 14. 
;; An iterative version of the factorial procedure using do
;; set! and reversed multiplication order.
(define (facdor n)
  (do ((r 1)
       (i 1 (+ i 1)))
       ((= n i) (* n r))
    (set! r (* i r))))


;; 15. 
;; An iterative version of the factorial procedure using
;; the "let loop" form and continuation-based exit.
(define (faci-exit n)
  (call/cc
   (lambda (k)
     (let loop ((r 1)
                (n n))
       (if (= 0 n) 
           (k r)
           (loop (* r n) (- n 1)))))))


;; 16. 
;; An iterative version of the factorial procedure using
;; the "let loop" form, continuation-based exit and 
;; reversed multiplication order.
(define (faci-exitr n)
  (call/cc
   (lambda (k)
     (let loop ((r 1)
                (m 1))
       (if (> m n) 
           (k r)
           (loop (* r m) (+ m 1)))))))


;; 17. 
;; A version using the Y combinator.
(define Y (lambda (f) 
            (lambda (v) ((f (y f)) v))))

(define fac-seed 
  (lambda (f) 
    (lambda (x) 
      (if (= x 1) 1 (* x (f (- x 1))))))) 

(define yfac (y fac-seed))


;; 18. 
;; A version using the U combinator.

(define (U f) (f f))

(define fac-seed-u 
  (lambda (f) 
    (lambda (x) 
      (if (= x 1) 1 (* x ((f f) (- x 1))))))) 

(define ufac (u fac-seed-u))


;; 19. 
;; An extremely inefficient version using unary numbers
;; represented as strings.
(define (fac-string n)
  (define (oloop str res)
    (if (= (string-length str) 1)
        (string-length res)
        (oloop (substring str 1 (string-length str))
              (let loop ((r res)
                         (s str))
                (if (= 1 (string-length s)) r
                    (loop (string-append r res)
                          (substring s 1 (string-length s))))))))
  (oloop (make-string n #\*) "*"))



;; Test them!

(when (not (= (fac 6)
              (facr 6)
              (faci 6)
              (facir 6)
              (facc 6 id)
              (faccr 6 id)
              (facl 6)
              (faclr 6)
              (facli 6)
              (faclir 6)
              (faclc 6)
              (faclcr 6)
              (facdo 6)
              (facdor 6)
              (faci-exit 6)
              (faci-exitr 6)
              (yfac 6)
              (ufac 6)
              (fac-string 6)))
      (error "The phase of the moon is ALL WRONG!"))
