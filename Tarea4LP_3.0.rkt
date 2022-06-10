#lang scheme

(define (suma_especial l a)
  (truncate
   (/
    (let sum ((i (length l))(n 0))
      (if (= i 0)
          n
          (sum (- i 1)(+ n (list-ref l (- i 1)))))) a)))


(define (merge_simple l1 l2 ops)
  (let ((lf (length l1)))
    (let rec ((i lf))
      (if (= i 0)
          '()
          (cons (cond
            ((equal? (list-ref ops (- lf i)) #\S) (+ (list-ref l1 (- lf i)) (list-ref l2 (- lf i))))
            ((equal? (list-ref ops (- lf i)) #\R) (- (list-ref l1 (- lf i)) (list-ref l2 (- lf i))))
            ((equal? (list-ref ops (- lf i)) #\M) (* (list-ref l1 (- lf i)) (list-ref l2 (- lf i))))
            (else #f)) (rec (- i 1)))))))
        

(define (merge_cola l1 l2 ops)
  (let ((lf (length l1)))
    (let rec ((i lf) (n '()))
      (if (= i 0)
          n
          (rec (- i 1)(append n (list(cond
            ((equal? (list-ref ops (- lf i)) #\S) (+ (list-ref l1 (- lf i)) (list-ref l2 (- lf i))))
            ((equal? (list-ref ops (- lf i)) #\R) (- (list-ref l1 (- lf i)) (list-ref l2 (- lf i))))
            ((equal? (list-ref ops (- lf i)) #\M) (* (list-ref l1 (- lf i)) (list-ref l2 (- lf i))))
            (else #f)))))))))


(define (demerge_simple l f)
  (list
   (let ((ln (length l)))
     (let rec ((i ln))
      (if (= i 0)
          '()
          (cons (f (list-ref l (- ln i))) (rec (- i 1))))))
     (let ((ln (length l))) 
      (let rec ((i ln))
      (if (= i 0)
          '()
          (cons (- (list-ref l (- ln i)) (f (list-ref l (- ln i)))) (rec (- i 1))))))))

 
(define (demerge_cola l f)
  (let ((ln (length l)))
    (let rec ((i ln) (n '()) (m '()))
      (if (= i 0)
          (list n m)
          (rec (- i 1)
            (append n (list(f (list-ref l (- ln i)))))
            (append m (list(- (list-ref l (- ln i)) (f (list-ref l (- ln i)))))))))))


(define (superior l ops f num)
  (let ((merge_res (merge_cola l l ops)))
    (let ((demerge_res (demerge_cola merge_res f)))
      (let
          ((r1 (* (suma_especial l num) 2))
           (r2 (+ (suma_especial (list-ref demerge_res 0) num) (suma_especial (list-ref demerge_res 1) num))))
        (if (< r2 r1)
            1
            0)))))


(define (all_superior matriz_ls matriz_ops matriz_f matriz_nums c f)
  (let rac ((j f) (m '()))
    (if (= j 0)
        m
        (rac (- j 1)
             (append m (list ((let rec ((i c) (n '()))
          (if (= i 0)
              n
              (rec (- i 1) (append n (list (superior
                                            (list-ref (list-ref matriz_ls (- f j)) (- c i))
                                            (list-ref (list-ref matriz_ops (- f j)) (- c i))
                                            (list-ref (list-ref matriz_f (- f j)) (- c i))
                                            (list-ref (list-ref matriz_nums (- f j)) (- c i)))))))))))))))


(all_superior
'(((1 2 3) (1 1 1)) ((2 2 2) (3 4 5)))
'(((#\S #\M #\R) (#\S #\S #\S)) ((#\R #\R #\R) (#\S #\S #\M)))
(list ((lambda (x) (- x 2)) (lambda (x) (modulo x 2))) ((lambda (x) (quotient x 2)) (lambda (x) (modulo x 2))))
'((2 3) (2 2))
2 2)