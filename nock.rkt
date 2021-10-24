#lang racket

(define (h noun) (list-ref noun 0))
(define (t noun) (list-ref noun 1))
(define (cell h t) (list h t))

(define (wut noun)
  (cond
    [(list? noun) 0]
    [else 1]
    ))

(define (lus noun)
  (cond
    [(number? noun) (+ noun 1)]
    ))

(define (tis noun)
  (cond
    [(and (number? (h noun)) (number? (t noun)))
     (if (= (h noun) (t noun)) 0 1)]
    [else
     (if (and (= (tis (cell (h (h noun)) (h (t noun)))) 0)
              (= (tis (cell (t (h noun)) (t (t noun)))) 0))
              0 1)]
    ))

(define (slot noun)
  (cond
    [(= 1 (h noun))
      (t noun)]
    [(= 2 (h noun))
      (h (t noun))]
    [(= 3 (h noun))
      (t (t noun))]
    [(= 0 (modulo (h noun) 2))
      (slot (cell 2 (slot (cell (/ (h noun) 2) (t noun)))))]
    [(= 1 (modulo (h noun) 2))
      (slot (cell 3 (slot (cell (/ (- (h noun) 1) 2) (t noun)))))]
    [else noun]
    ))

(define (edit noun)
  (cond
    [(= 1 (h noun))
      (h (t noun))]
    [(= 0 (modulo (h noun) 2))
       (local ([define a (/ (h noun) 2)]
               [define b (h (t noun))]
               [define c (t (t noun))])
       (edit (cell a (cell (cell b
             (slot (cell (+ a a 1) c))) c))))]
    [(= 1 (modulo (h noun) 2))
       (local ([define a (/ (- (h noun) 1) 2)]
               [define b (h (t noun))]
               [define c (t (t noun))])
       (edit (cell a (cell (cell
             (slot (cell (+ a a) c)) b) c))))]
    [else noun]
    ))


(define (nock noun)
  (local ([define a (h noun)]
          [define op (h (t noun))])
  (cond
    [(list? op)
      (local ([define c (t (t noun))])
      (cell (nock (cell a op)) (nock (cell a c))))]
    [(= op 0)
      (local ([define b (t (t noun))])
      (slot (cell b a)))]
    [(= op 1)
      (local ([define b (t (t noun))])
      b)]
    [(= op 2)
      (local ([define b (h (t (t noun)))]
              [define c (t (t (t noun)))])
      (nock (cell (nock (cell a b)) (nock (cell a c)))))]
    [(= op 3)
       (local ([define b (t (t noun))])
       (wut (nock (cell a b))))]
    [(= op 4)
       (local ([define b (t (t noun))])
       (lus (nock (cell a b))))]
    [(= op 5)
       (local ([define b (h (t (t noun)))]
               [define c (t (t (t noun)))])
       (tis (cell (nock (cell a b)) (nock (cell a c)))))]
    [(= op 6)
       (local ([define b (h (t (t noun)))]
               [define c (h (t (t (t noun))))]
               [define d (t (t (t (t noun))))])
       (nock (cell a (nock (cell (cell c d)
                                 (cell 0
                                 (nock (cell (cell 2 3)
                                             (cell 0
                                             (nock (cell a (cell 4 (cell 4 b))))))
                                       )))
                           )))
         )]
    [(= op 7)
       (local ([define b (h (t (t noun)))]
               [define c (t (t (t noun)))])
       (nock (cell (nock (cell a b)) c)))]
    [(= op 8)
       (local ([define b (h (t (t noun)))]
               [define c (t (t (t noun)))])
       (nock (cell (cell (nock (cell a b)) a) c)))]
    [(= op 9)
       (local ([define b (h (t (t noun)))]
               [define c (t (t (t noun)))])
       (nock (cell (nock (cell a c))
                   (cell 2 (cell (cell 0 1) (cell 0 b))))))]
    [(= op 10)
       (local ([define b (h (h (t (t noun))))]
               [define c (t (h (t (t noun))))]
               [define d (t (t (t noun)))])
       (edit (cell b (cell (nock (cell a c)) (nock (cell a d))))))]
    [(and (= op 11) (list? (h (t (t noun)))))
       (local ([define b (h (h (t (t noun))))]
               [define c (t (h (t (t noun))))]
               [define d (t (t (t noun)))])
       (nock (cell (cell (nock (cell a c)) (nock (cell a d))) (cell 0 3))))]
    [(= op 11)
       (local ([define b (h (t (t noun)))]
               [define c (t (t (t noun)))])
       (nock (cell a c)))]
    [else noun]
    )))


; end of interpreter
; ; ; ; ; ; ; ; ; ; ;


(define (test noun truth)
  (if (= 0 (tis (cell noun truth)))
      (display "   pass.\n")
      (display "   fail.\n")))

(define subject (cell (cell 11 22) (cell (cell 33 44) 55)))
                                          ; [[11 2] [33 44] 55]

; begin tests
(display "simple test cases:\n\n")

(display "  nock 0:\n")
  (test (nock (cell subject (cell 0 1)))  ; .*(subject [0 1])
        subject)                          ; subject

  (test (nock (cell subject (cell 0 2)))  ; .*(subject [0 2])
        (cell 11 22))                     ; [11 22]

  (test (nock (cell subject (cell 0 4)))  ; .*(subject [0 4])
        11)                               ; 11

  (test (nock (cell subject (cell 0 3)))  ; .*(subject [0 3])
        (cell (cell 33 44) 55))           ; [[33 44] 55]


(display "\n  nock 1:\n")
  (test (nock (cell 0 (cell 1 33)))       ; .*(0 [1 33])
       33)                                ; 33


(display "\n  nock 2:\n")
  (test (nock (cell (cell
              (cell 4 (cell 0 1)) 14)
              (cell 2 (cell (cell 0 3)
              (cell 0 2)))))              ; .*([[4 [0 1]] 14] [2 [0 3] [0 2]])
        15)                               ; 15


(display "\n  nock 3:\n")
  (test (nock (cell subject
              (cell 3 (cell 0 1))))       ; .*(subject [3 0 1])
        0)                                ; 0

  (test (nock (cell subject
              (cell 3 (cell 0 4))))       ; .*(subject [3 0 4])
        1)                                ; 1


(display "\n  nock 4:\n")
  (test (nock (cell subject
              (cell 4 (cell 0 4))))       ; .*(subject [4 0 4])
        12)                               ; 12


(display "\n  nock 5:\n")
  (test (nock (cell subject
              (cell 5 (cell
              (cell 0 2) (cell
               1 (cell 11 22))))))        ; .*(subject [5 [0 2] 1 [11 22]])
        0)                                ; 0
  
  (test (nock (cell subject
              (cell 5 (cell (cell 0 2)
              (cell 1
              (cell 11 23))))))           ; .*(subject [5 [0 2] 1 [11 23]])
        1)                                ; 1


(display "\n  nock 6:\n")
  (test (nock (cell subject
              (cell 6 (cell (cell 1 0)
              (cell (cell 0 4)
              (cell 0 12))))))            ; .*(subject [6 [1 0] [0 4] [0 12]])
        11)                               ; 11

  (test (nock (cell subject
              (cell 6 (cell (cell 1 1)
              (cell (cell 0 4)
              (cell 0 12))))))            ; .*(subject [6 [1 0] [0 4] [0 12]])
        33)                               ; 33

  

(display "\n  nock 7:\n")
  (test (nock (cell subject
              (cell 7 (cell
              (cell 0 12) (cell
               4 (cell 0 1))))))          ; .*(subject [7 [0 12] 4 0 1])
        34)                               ; 34



(display "\n  nock 8:\n")
(test (nock (cell subject
            (cell 8 (cell
            (cell 4 (cell 0 13))
            (cell 0 1)))))                ; .*(subject [8 [4 0 13] [0 1]])
      (cell 45 subject))                  ; [45 subject]


(display "\n  nock 9:\n")
(test (nock (cell (cell 68
            (cell 4 (cell 0 3)))
            (cell 9 (cell 2 (cell
            (cell 0 3) (cell 0 2))))))    ; .*([68 4 0 3] [9 2 [0 3] [0 2]])
      69)                                 ; 69
                                          ; B-)

(display "\n  nock 10:\n")
(test (nock (cell (cell 11 22)
            (cell 10 (cell
            (cell 2 (cell 0 1))
            (cell 0 1)))))                ; .*([11 22] [10 [2 [0 1]] 0 1])
      (cell (cell 11 22) 22))             ; [[11 22] 22]


(display "\n  nock 11:\n")
(test (nock (cell 0 (cell 11 (cell
            (cell (cell 1 100)
                  (cell 1 200))
            (cell 1 300)))))              ; .*(0 [11 [[1 100] [1 200]] [1 300]])
      300)                                ; 300

(test (nock (cell subject
            (cell 11
            (cell 69 (cell 0 1)))))       ; .*(subject [11 69 [0 1]])
      subject)                            ; 69
                                          ; B-)

(display "\n\ndecrement on 70:\n")
(define dec (cell 8 (cell (cell 1 0) (cell 8 (cell (cell 1 (cell 6 (cell (cell 5 (cell (cell 0 7) (cell 4 (cell 0 6)))) (cell (cell 0 6) (cell 9 (cell 2 (cell (cell 0 2) (cell (cell 4 (cell 0 6)) (cell 0 7))))))))) (cell 9 (cell 2 (cell 0 1))))))))
(test (nock (cell 70 dec)) 69)  ; B-)
