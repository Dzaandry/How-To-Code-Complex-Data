;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname maze-2w-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Solve simple square mazes

;; maze-v1.rkt


;; Constants:

;; Data definitions:

;; Maze is (listof Boolean)
;; interp. a square maze
;;         each side length is (sqrt (length <maze>))
;;         true  (aka #t) means open, can move through this
;;         false (aka #f) means a wall, cannot move into or through a wall
;;

(define O #t) ;Open
(define W #f) ;Wall

(define M0
  (list O W W W
        W W W W
        W W W W
        W W W W))

(define M1
  (list O W W W W
        O O W O O
        W O W W W 
        O O W W W
        O O O O O))

(define M2
  (list O O O O O
        O W W W O
        O W W W O
        O W W W O
        O W W W O))

(define M3            
  (list O O O O O
        O W W W W
        O W W W W
        O W W W W 
        O O O O O))

(define M4
  (list O O O O O
        O W W W O
        O W O O O
        O W O W W
        W W O O O))

(define-struct pos (x y))
;; Pos is (make-pos Integer Integer)
;; interp. an x, y position in the maze.
;;         0, 0 is upper left.
;;         a position is only valid for a given maze if:
;;            - (<= 0 x (sub1 <size>))
;;            - (<= 0 y (sub1 <size>))
;;            - there is a true in the given cell
;;
(define P0 (make-pos 0 0)) ;upper left  in 4x4 maze
(define P1 (make-pos 3 0)) ;upper right  "  "   "
(define P2 (make-pos 0 3)) ;lower left   "  "   "
(define P3 (make-pos 3 3)) ;lower left   "  "   "


;; Functions:

;; Maze Pos -> Boolean
;; produce contents of given square in given maze
(check-expect (mref (list #t #f #f #f) (make-pos 0 0)) #t)
(check-expect (mref (list #t #t #f #f) (make-pos 0 1)) #f)

(define (mref m p)
  (local [(define s (sqrt (length m))) ;each side length
          (define x (pos-x p))
          (define y (pos-y p))]
    
    (list-ref m (+ x (* y s)))))



;; Maze Pos -> Boolean
;; produce true if the maze is solvable
;; solvable means it's possible to go from upper-left to lower-right given you can only move down or right
(check-expect (solvable? M0) false)
(check-expect (solvable? M1) true)
(check-expect (solvable? M2) true)
(check-expect (solvable? M3) true)
(check-expect (solvable? M4) false)

(define (solvable? m)
  (local [(define (solvable--pos? p)
            (if (solved? m p)
                true
                (solvable--lop? (next-poses m p))))
          
          (define (solvable--lop? lop)
            (cond [(empty? lop) false]
                  [else 
                     (if (solvable--pos? (first lop))
                         true
                         (solvable--lop? (rest lop)))]))]
    
    (solvable--pos? P0)))


;; Maze Pos -> Boolean
;; produce true if the given maze is solved
;; solved means pos is lower right (pos-x = (sqrt (length m)) - 1),
;;                                 (pos-y = pos-x)
(check-expect (solved? (list O) (make-pos 0 0)) true)
(check-expect (solved? (list O W W O) (make-pos 0 0)) false)
(check-expect (solved? (list O W W O) (make-pos 1 1)) true)

(define (solved? m p)
  (and (= (pos-x p) (- (sqrt (length m)) 1))
       (= (pos-x p) (pos-y p))))
 
;; Maze Pos -> (listof Pos)
;; produce up to 2 next viable Poses in the maze
(check-expect (next-poses M0 (make-pos 0 0)) empty)
(check-expect (next-poses M1 (make-pos 0 0)) (list (make-pos 0 1)))
(check-expect (next-poses M2 (make-pos 0 0)) (list (make-pos 1 0) (make-pos 0 1)))
(check-expect (next-poses M2 (make-pos 5 5)) empty)

(define (next-poses m p)
  (local [(define maxX (- (sqrt (length m)) 1))
          (define maxY maxX)
          (define (viable? p)
            (and (<= (pos-x p) maxX)
                 (<= (pos-y p) maxY)
                 (mref m p)))]
            
  (filter viable? (list (make-pos (add1 (pos-x p)) (pos-y p))
                        (make-pos (pos-x p) (add1 (pos-y p)))))))


(define G (square 50 "solid" "grey"))
(define B (square 50 "solid" "black"))
(define BLANK (square 0 "solid" "white"))
(beside G B B G)


;; Maze -> Image
;; produce a simple rendering of a Maze
(define (render-maze m)
  (local [(define SIZE (sqrt (length m)))
          (define IDXS (build-list SIZE identity))
          (define (cv->img cv)
            (cond [cv G]
                  [else B]))]
    (foldr (lambda (i img)
             (above (foldr (lambda (j img)
                             (beside (cv->img (list-ref m (+ (* i SIZE) j))) img))
                           BLANK
                           IDXS)
                    img))
           BLANK IDXS)))

(render-maze M1)
                           