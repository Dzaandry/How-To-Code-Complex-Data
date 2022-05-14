;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nqueens-mine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; nqueens-v1.rkt
;;
;; This version has data definitions, signature, purpose, tests and a couple
;; helpers.


;; Data definitions:

;; Position is Natural
;; interp. positions on the board
;;         if    N is the number of queens
;;         then  (sqr N) is the number of positions on the board
;;         so    this number should be in [0, (- (sqr N) 1)]
(define P1 0)        ;upper left corner of board
(define P2 (- 16 1)) ;lower right corner of 4x4 board

;; Board is (listof Position)  up to N elements long
;; interp. the positions of the queens that have been placed on the board
(define BD1 empty)           ;no queens placed
(define BD2 (list 0))        ;one queen in upper left corner
(define BD3 (list 14 8 7 1)) ;a solution to 4x4 puzzle 




;; Functions:

;; NOTE about following function. It could have been designed with N as a
;; top-level constant and all the locally defined functions as top-level
;; functions. But doing it the way we have done below, makes it easy to make
;; the top-level nqueens function consume N which is kind of nice.  The
;; trampoline starts the actual search out by calling fn-for-bd with an empty
;; board.

;; Natural -> Board or false)
;; produce first found solution for n queens of size N; or false if none exists
(check-expect (nqueens 1) (list 0))
(check-expect (nqueens 2) false)
(check-expect (nqueens 3) false)
(check-expect (nqueens 4) (list 1 7 8 14))
(check-expect (nqueens 5) (list 0 7 14 16 23))
(check-expect (nqueens 6) (list 1 9 17 18 26 34))
(check-expect (nqueens 7) (list 0 9 18 27 29 38 47))
(check-expect (nqueens 8) (list 0 12 23 29 34 46 49 59))


(define (nqueens N)
  (local [(define (fn-for-bd bd)
            (if (solved? bd)
                bd
                (fn-for-lobd (board-subs bd))))

          (define (fn-for-lobd lobd)
            (cond [(empty? lobd) false]
                  [else
                   (local [(define try (fn-for-bd (first lobd)))]
                     (if (not (false? try))
                         try
                         (fn-for-lobd (rest lobd))))]))


          
          ;; Position Position -> Boolean
          ;; produce true if queens at position a and b attack each other
          (define (attack? pa pb)
            (local [(define x1 (pos-x pa))
                    (define y1 (pos-y pa))
                    (define x2 (pos-x pb))
                    (define y2 (pos-y pb))]
              (or (= x1 x2)                           ;same row
                  (= y1 y2)                           ;same column
                  (= (/ (- y2 y1) (- x2 x1))  1)      ;same slope  1 diagonal
                  (= (/ (- y2 y1) (- x2 x1)) -1))))   ;same slope -1 diagonal
          

          ;; Pos -> Natural[0, N)
          ;; produce the row or column number for the given position
          (define (pos-x p) (remainder p N))
          (define (pos-y p) (quotient  p N))

          

          ;; Board -> Boolean
          ;; produce true if the Board is solved
          (define (solved? bd)
            (= N (length bd)))

          ;;Board -> (listof Board)
          ;; produce the next Boards
          (define (board-subs bd)
            (keep-only-valid (fill-all bd)))

          ;;Board -> (listof Board)
          ;; produce all possible next boards
          (define (fill-all bd)
            (local [(define (find-last bd)
                      (cond [(empty? bd) -1]
                            [(empty? (rest bd)) (first bd)]
                            [else
                             (find-last (rest bd))]))
                    (define LAST (find-last bd))
                    (define MAX-POS (- (sqr N) 1))
                    
                    (define (from-last-to-max l)
                      (if (= (- MAX-POS l) 0)
                          empty
                          (cons (add1 l)
                                (from-last-to-max (add1 l)))))
                    (define (local-fill-all lov)
                      (cond [(empty? lov) empty]
                            [else
                             (cons (append bd (list (first lov)))
                                   (local-fill-all (rest lov)))]))]
              (local-fill-all (from-last-to-max LAST))))

          ;;(listof Board) -> (listof Board)
          ;; filter out invalid boards
          (define (keep-only-valid lobd)
            (local [(define (valid? bd)
                      (local [(define (find-last bd)
                                (if (empty? (rest bd))
                                    (first bd)
                                    (find-last (rest bd))))
                              (define LAST (find-last bd))]
                        (cond [(empty? (rest bd)) true]
                              [else
                               (if (attack? (first bd) LAST)
                                   false
                                   (valid? (rest bd)))])))]
              (filter valid? lobd)))]

          
    (fn-for-bd empty)))



