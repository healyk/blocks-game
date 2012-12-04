;;;
;;; Constants
;;;
(define board-width 10)
(define board-height 20)

(define piece-pixel-size 16)

;;;
;;; Board data structure
;;;

(define (render-board-background sprites x-off y-off width height)
  (do ((y -1 (+ 1 y)))
      ((= y (+ 1 height)))
    (do ((x -1 (+ 1 x)))
        ((= x (+ 1 width)))
      (let ((sprite (if (not (pair? sprites)) 
                        sprites
                        (list-ref sprites 
                                  (random-integer (- (length sprites) 1))))))
        (render-sprite sprite
                       (+ x-off (* x piece-pixel-size))
                       (+ y-off (* y piece-pixel-size))
                       color:
                       (if (or (= x -1) (= y -1)
                               (= x width) (= y height))
                           '(255 255 255 255)
                           '(128 128 128 255)))))))

;; Gets the value of a board square
(define (board/get-square board x y)
  (vector-ref board
              (+ (* y board-width) x)))

;; Sets a square on the board to a value
(define (board/set-square board x y value)
  (vector-set! board
               (+ (* y board-width) x)
               value))

;; Draws the board to the screen.
(define (board/render board x-off y-off)
  (do ((y 0 (+ 1 y)))
      ((eq? y board-height))
    (do ((x 0 (+ 1 x)))
        ((eq? x board-width))
      (let ((piece (board/get-square board x y)))
        (if (not (null? piece))
            (render-sprite piece
                           (+ x-off (* x piece-pixel-size))
                           (+ y-off (* y piece-pixel-size))))))))

;; Structure used to model a game.
(define-structure tris-game
  score
  level
  lines
  board

  next-piece
  current-piece
  current-position

  sprites
  time-till-drop
  flags)

;; Creates a new game with an empty board.
(define (new-game)
  (make-tris-game 0
                  1
                  0
                  (make-vector (* board-width board-height) '())
                  (piece/generate-random (get-random-sprite block-sprites))
                  (piece/generate-random (get-random-sprite block-sprites))
                  (list 4 0)
                  block-sprites
                  1100
                  '()))

(define (game/has-flag? game flag)
  (pair? (assq flag (tris-game-flags game))))

(define (game/add-flag! game flag . value)
  (tris-game-flags-set! game
                   (append (tris-game-flags game)
                           (list
                            (if (not (null? value))
                                (list flag (car value))
                                (list flag '()))))))

(define (game/clear-flag game flag)
  (tris-game-flags-set! game
                   (filter (lambda (x) (not (eq? (car x) flag)))
                           (tris-game-flags game))))

(define (get-random-sprite sprites)
  (get-sprite sprites (+ 1 (random-integer 6)) 0))

;; Modifies the score by n.  Note that n will be modified based on level
;; as well.
;; Type is the type of scoring that has occured:
;;   * soft-drop -> Piece was placed by holding down (not impl'ed yet)
;;   * hard-drop -> Player hit the drop-down button, instantly dropping the
;;                  piece.
;;   * single    -> Single line clear
;;   * double    -> 2 lines clear
;;   * triple    -> 3 lines clear
;;   * quad      -> 4 lines clear
(define (game/mod-score! game type . height)
  (if (not (game/has-flag? game 'game-over))
      (let* ((level (tris-game-level game))
             (score (tris-game-score game)))
        (tris-game-score-set! game 
                              (+ score
                                 (case type
                                   ((hard-drop) (* 2 (car height) level))
                                   ((single)    (* 40 level))
                                   ((double)    (* 100 level))
                                   ((triple)    (* 300 level))
                                   ((quad)      (* 1200 level))
                                   (else level)))))))

;; Test to see if a piece can be placed at the given position.  If there are
;; any blocking squares this returns false.
(define (game/can-move-piece? game piece pos)
  (let ((board (tris-game-board game))
        (pos-x (car pos))
        (pos-y (cadr pos))
        (piece-width  (piece/get-width piece))
        (piece-height (piece/get-height piece)))
    (begin
      ; Check for colliding board positions.
      (let loop ((x 0)
                 (y 0))
        (cond
         ((and (< y piece-height)
               (< x piece-width)
               (or (or (< (+ x pos-x) 0)
                       (< (+ y pos-y) 0)
                       (>= (+ pos-x x) board-width)
                       (>= (+ pos-y y) board-height))
                   (not (null? (board/get-square board 
                                                 (+ x pos-x) 
                                                 (+ y pos-y)))))
               (piece/get-square piece x y))
          
          #f)
         ((< y piece-height) (loop x (+ 1 y)))
         ((< x piece-width)  (loop (+ 1 x) 0))
         (else #t))))))

;; Recacluates the time till a piece lowers by a cell.
(define (game/recalc-time! game)
  (let* ((time (* (- 11 (tris-game-level game)) 100)))
    (tris-game-time-till-drop-set! game time)))

;; Performs an action for the current game.  The current actions are:
;;   * move-left
;;   * move-right
;;   * rotate-right
;;   * rotate-left
;;   * move-down - moves down one block
;;   * drop-piece - moves the piece all the way down.
(define (game/action! game action)
  ;; Finds the lowest position the piece can be placed
  (define (find-drop-position game)
    ; Start at the bottom of the board and iterate up until we find the correct
    ; position
    (let* ((piece        (tris-game-current-piece game))
           (piece-width  (piece/get-width piece))
           (piece-height (piece/get-height piece))
           (x-pos        (car (tris-game-current-position game)))
           (y-pos        (cadr (tris-game-current-position game))))
      (list x-pos
            (let loop-y ((y 0))
              (if (<= y (+ piece-height board-height))
                  (if (game/can-move-piece? game piece (list x-pos y))
                      (loop-y (+ y 1))
                      (- y 1))
                  y-pos)))))

  ;; Depending on the action this will tranform the old position into a new
  ;; position
  (define (get-move-position old-pos)
    (case action
      ((move-left)  (list (- (car old-pos) 1) (cadr old-pos)))
      ((move-right) (list (+ (car old-pos) 1) (cadr old-pos)))
      ((move-down)  (list (car old-pos) (+ (cadr old-pos) 1)))
      ((drop-piece) (find-drop-position game))
      (else old-pos)))
  
  (if (not (game/has-flag? game 'game-over))
      (let* ((old-piece (tris-game-current-piece game))
             (old-pos   (tris-game-current-position game))
             (new-piece (case action
                          ((rotate-right) (piece/rotate-right old-piece))
                          ((rotate-left)  (piece/rotate-left old-piece))
                          (else old-piece)))
             (new-pos   (get-move-position old-pos)))
        (if (game/can-move-piece? game new-piece new-pos)
            (begin
              (if (not (eq? new-piece old-piece))
                  (tris-game-current-piece-set! game new-piece))
              (if (not (eq? new-pos old-pos))
                  (tris-game-current-position-set! game new-pos))
              ; Special - recalc time for some cases
              (if (or (eq? action 'move-down) (eq? action 'drop-piece))
                  (game/recalc-time! game))

              ; Special - adjust for a hard drop
              (if (eq? action 'drop-piece)
                  (begin
                    (game/place-piece! game new-piece)
                    (game/mod-score! game 'hard-drop 
                                     (- (cadr new-pos) (cadr old-pos))))))))))

;; Returns a list of rows that are full (have nothing but blocks in them)
(define (game/find-full-rows game)
  (let ((board (tris-game-board game)))
    (let loop-y ((y 0)
                 (rows '()))
      (if (< y board-height)
          (let loop-x ((x 0))
            (if (< x board-width)
                (if (null? (board/get-square board x y))
                    (loop-y (+ y 1) rows)
                    (loop-x (+ x 1)))
                (loop-y (+ y 1) (cons y rows))))
          rows))))

;; Removes the given row from the board (removes all square data).  This moves
;; all rows above this row down by 1.
(define (game/remove-row! game row)
  (let ((row-start (* row board-width))
        (row-end   (* (+ 1 row) board-width))
        (board-lst (vector->list (tris-game-board game))))
    (tris-game-board-set! game
                          (list->vector
                           (append
                            ; Add a new empty row to the top
                            (make-list board-width '())
                            (take board-lst row-start)
                            (drop board-lst row-end))))))

(define (game/add-new-piece! game)
  (let ((new-piece (piece/generate-random
                    (get-random-sprite (tris-game-sprites game))))
        (new-pos   (list 4 0)))
    ; Test for game-over
    (if (game/can-move-piece? game new-piece new-pos)
        (begin
          (tris-game-current-piece-set! game (tris-game-next-piece game))
          (tris-game-next-piece-set! game new-piece)
          (tris-game-current-position-set! game new-pos))
        (begin
          (game/add-flag! game 'game-over)
          (gamestate-switch 
           (simple-prompt-state "Gameover" 
                                (if (highscore/is-new? (tris-game-score game))
                                    (new-highscore-state (tris-game-score game))
                                    mainmenu-state)))))))

;; Used to place a piece on the game board.  This will fill the squares with
;; the required sprites.  It also takes care of all the game book-keeping when
;; a piece is placed, generating a new piece, removing full rows and so forth.
(define (game/place-piece! game piece)
  (define (score-rows full-rows)
    ; Make sure we adjust for each row we remove individually.  Since
    ; we remove row n, the next row will have to know it's n+ [closer
    ; to the bottom of the board].
    (for-each (lambda (row-num offset) 
                (game/remove-row! game (+ row-num offset))) 
              full-rows
              (iota (length full-rows) 0))

    ; Set our new number of rows and recalculate the level      
    (tris-game-lines-set! game (+ (tris-game-lines game) (length full-rows)))
    (tris-game-level-set! game 
                          (min (+ 1 (quotient (tris-game-lines game) 10)) 10))
    (game/mod-score! game 
                     (case (length full-rows)
                       ((1) 'single)
                       ((2) 'double)
                       ((3) 'triple)
                       ((4) 'quad))))

  (let ((board (tris-game-board game))
        (pos-x (car (tris-game-current-position game)))
        (pos-y (cadr (tris-game-current-position game))))
    (do ((x 0 (+ 1 x)))
        ((= x (piece/get-width piece)))
      (do ((y 0 (+ 1 y)))
          ((= y (piece/get-height piece)))
        (if (piece/get-square piece x y)
            (board/set-square board 
                              (+ pos-x x) 
                              (+ pos-y y) 
                              (car piece)))))
    ; Clear out full rows
    (let ((full-rows (game/find-full-rows game)))
      (if (not (null? full-rows))
          (begin
            (sound/play clear-row-sound)
            (score-rows full-rows))
          (sound/play place-piece-sound)))
    (game/add-new-piece! game)))

;; reset the drop time after a piece has moved down
(define (game/drop-piece! game)
  ; Calculate the new time
  (let* ((pos     (tris-game-current-position game))
         (new-pos (list (car pos) (+ 1 (cadr pos))))
         (piece   (tris-game-current-piece game)))
    (game/recalc-time! game)

    ; Check to see if we can drop the piece.  If not place the piece
    (if (game/can-move-piece? game piece new-pos)
        (tris-game-current-position-set! game (list (car pos)
                                                    (+ 1 (cadr pos))))
        (begin (game/place-piece! game piece)
               (game/mod-score! game 'soft-drop 1)))))

(define (game/update-drop-time! game delta)
  (tris-game-time-till-drop-set! game 
                                 (- (tris-game-time-till-drop game) delta)))

(define (game/drop-piece? game)
  (<= (tris-game-time-till-drop game) 0))
