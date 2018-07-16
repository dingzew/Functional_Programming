#lang racket

(provide (all-defined-out))

; author: Dingze Wang
; UW CSE341 programming languages hw4

; 1. concat everything from low to high in a list
(define (sequence low hi stride)
  (letrec ([f (lambda (x)
              (if (<= x hi)
                  (cons x  (f (+ x stride)))
                  null))])
    (f low)))


; 2. add a suffix after each string in the list
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))


; 3. get the n-th value in the list, if n > list.length, return (n % list.length)-th element
(define (list-nth-mod xs n)
  (if (= (length xs) 0)
      (error "list-nth-mod: empty list")
        (if (< n 0)
            (error "list-nth-mod: negative number")
            (car (list-tail xs (remainder n (length xs)))))))
         

; 4. return the first n values in the stream as a list
(define (stream-for-n-steps s n)
  (letrec ([f (lambda (stream count)
                (if (< count n)
                (cons (car (stream)) (f (cdr (stream)) (+ 1 count)))
                null))])
    (f s 0)))


; 5. if the index % 5 == 0, then multiple it by -1
(define funny-number-stream
  (letrec ([f (lambda (count)
                (if (= 0 (remainder count 5))
                    (cons (- 0 count) (lambda () (f (+ count 1))))
                    (cons count  (lambda () (f (+ count 1))))))])
           (lambda () (f 1))))


; 6. return a stream show dan.jpg and dog.jpg back and force
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))


; 7. return the stream that modify every element to a pair (0. original_value)
(define (stream-add-zero s)
  (letrec ([f (lambda (stream)
                (cons (cons 0 (car (stream))) (lambda () (f (cdr (stream))))))])
    (lambda () (f s))))


; 8. combine elements in two lists as pair stream based on list-nth-mod
(define (cycle-lists xs ys)
  (letrec ([f (lambda (count)
                (cons (cons (list-nth-mod xs count) (list-nth-mod ys count)) (lambda () (f (+ 1 count)))))])
    (lambda () (f 0))))

; 9. run assoc on a vector 
(define (vector-assoc v vec)
  (letrec ([f (lambda (count)
                (if (>= count (vector-length vec))
                    #f
                    (if (pair? (vector-ref vec count))
                        (if (equal? (car (vector-ref vec count)) v)
                        (vector-ref vec count)
                        (f (+ 1 count)))
                        (f (+ 1 count)))))])                
    (f 0)))


; 10. use our own cache to run assoc before calling assoc
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
    [index 0])
    (lambda (v)
      (if (vector-assoc v cache)
          (car (vector-assoc v cache))
          (if (assoc v xs)
              (begin (vector-set! cache index (assoc v xs))
                     (set! index (remainder (+ 1 index) n))
                     (assoc v xs))
              #f)))))


; challenge problem: if (e2) is less than (e1) then run e2 until (e2) is greater or equal than (e1)
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([exp1 e1]
              [exp2 (lambda () e2)]
              [loop (lambda ()
                      (if (< (exp2) exp1)
                          (loop)
                          #t))])
       (loop))]))
       