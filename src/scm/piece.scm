;;;
;;; Piece data structure
;;;
;;; The piece data-type is a list.  The first element is the sprite, the second
;;; element is a list containing the size (width, height) and the third element
;;; is a vector describing the physical shape of the piece.
;;;

(define piece-j
  (list '(3 3)
        '#(#f #t #f
           #f #t #f
           #t #t #f)))

(define piece-l
  (list '(3 3)
        '#(#f #t #f
           #f #t #f
           #f #t #t)))

(define piece-o
  (list '(2 2)
        '#(#t #t
           #t #t)))

(define piece-i
  (list '(4 4)
        '#(#f #t #f #f
           #f #t #f #f
           #f #t #f #f
           #f #t #f #f)))

(define piece-s
  (list '(3 3)
        '#(#f #t #t
           #t #t #f
           #f #f #f)))

(define piece-z
  (list '(3 3)
        '#(#t #t #f
           #f #t #t
           #f #f #f)))

(define piece-t
  (list '(3 3)
        '#(#f #t #f
           #t #t #t
           #f #f #f)))

;; Generates a random tetronome.
(define (piece/generate-random sprite)
  (piece/make 
   (case (random-integer 7)
     ((0) piece-j)
     ((1) piece-l)
     ((2) piece-o)
     ((3) piece-i)
     ((4) piece-s)
     ((5) piece-z)
     ((6) piece-t)
     (else (raise "Got number out of range")))
   sprite))

(define (piece/get-width piece)
  (caadr piece))

(define (piece/get-height piece)
  (cadr (cadr piece)))

(define (piece/get-square piece x y)
  (vector-ref (caddr piece)
              (+ x (* y (piece/get-width piece)))))

;; This will make a new piece from a pattern and given sprite. 
(define (piece/make pattern sprite)
  (cons sprite pattern))

(define (piece/render-to-board piece x-off y-off x-board y-board)
  (let ((width   (piece/get-width piece))
        (height  (piece/get-height piece))
        (pixel-x (+ x-off (* x-board piece-pixel-size)))
        (pixel-y (+ y-off (* y-board piece-pixel-size)))
        (sprite  (car piece)))
    (do ((y 0 (+ y 1)))
        ((= y height))
      (do ((x 0 (+ x 1)))
          ((= x width))
        (let ((square (piece/get-square piece x y)))
          (if square
              (render-sprite sprite
                             (+ pixel-x (* x piece-pixel-size))
                             (+ pixel-y (* y piece-pixel-size)))))))))

;; Takes a piece, rotates it 90 degrees to the right, and returns a new piece.
(define (piece/rotate-right piece)
  (define (transpose matrix width height)
    (let ((new-matrix (make-vector (* width height) #f)))
      (do ((x 0 (+ 1 x)))
          ((= x width))
        (do ((y 0 (+ 1 y)))
            ((= y height))
          (vector-set! new-matrix 
                       (+ y (* x height))
                       (vector-ref matrix (+ x (* y width))))))
      new-matrix))

  (define (reverse-rows matrix width height)
    (let ((new-matrix (make-vector (* width height) #f)))
      (do ((x 0 (+ 1 x)))
          ((>= x  (/ width 2)))
        (do ((y 0 (+ 1 y)))
            ((= y height))
          (vector-set! new-matrix
                       (+ (- width x 1) (* y width))
                       (vector-ref matrix (+ x (* y width))))
          (vector-set! new-matrix
                       (+ x (* y width))
                       (vector-ref matrix (+ (- width x 1) (* y width))))))
      new-matrix))

  (let ((width  (car (cadr piece)))
        (height (cadr (cadr piece))))
    (list (car piece)
          (list height width)
          (reverse-rows (transpose (caddr piece) width height) height width))))

(define (piece/rotate-left piece)
  (piece/rotate-right (piece/rotate-right (piece/rotate-right piece))))
