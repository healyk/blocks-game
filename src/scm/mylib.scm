;;;;
;;;; mylib.scm
;;;;
;;;; Contains the hooks for the c calls.  It also contains other misc 
;;;; functionality that doesn't really belong anywhere else.
;;;;

;; Initialize the random number generator
(random-source-randomize! default-random-source)

(define block-sprites #f)
(define clear-row-sound #f)
(define place-piece-sound #f)

(define game #f)
(define game-is-running #t)

;;; 
;;; highscore
;;;
(define highscore-filename "scores.dat")
(define highscores '(("PostageStamp" 10000)
                     ("Vega"         9000)
                     ("Octokevin"    8000)
                     ("Cutedge"      7000)
                     ("Chocolate"    6000)
                     ("Skim"         5000)
                     ("Brain"        4000)
                     ("Bacon"        3000)
                     ("Bull"         2000) 
                     ("Phoebe"       1000)))
(define highscore/max-name-length 20)

(define (highscore/init)
  (if (file-exists? highscore-filename)
      (call-with-input-file highscore-filename
        (lambda (in)
          (set! highscores (read in))))))

(define (highscores/write)
  (call-with-output-file highscore-filename
    (lambda (out)
      (write highscores out))))

(define (highscore/is-new? score)
  (< (cadar (drop highscores 9)) score))

(define (highscore/add! name score)
  ; Check to see if this is a new high score first
  (if (highscore/is-new? score)
      (begin
        (set! highscores
              (sort (cons (list name score) 
                          (take highscores 9))
                    (lambda (a b)
                      (> (cadr a) (cadr b)))))
        (highscores/write))))

;; Renders an in-game game to the screen
(define (render-game game)
  (let ((x-off 32)
        (y-off 32))
    (render-board-background (get-sprite block-sprites 0 0) 
                             x-off y-off
                             board-width board-height)
    (board/render (block-game-board game) x-off y-off)
    (let ((piece-pos (block-game-current-position game)))
      (piece/render-to-board (block-game-current-piece game) 
                            x-off y-off
                            (car piece-pos)
                            (cadr piece-pos))
      ; Render next piece
      (piece/render-to-board (block-game-next-piece game)
                             (+ x-off (* board-width piece-pixel-size) 32)
                             (+ y-off 16)
                             0 0)
      ; Render ghost piece
      (if (game/has-option? game 'ghost)
          (let ((ghost-pos (game/find-drop-position game)))
            ; Make sure the ghost piece is 'below' our current piece
            (if (< (cadr piece-pos) (cadr ghost-pos))
                (piece/render-to-board (block-game-current-piece game)
                                       x-off y-off
                                       (car ghost-pos)
                                       (cadr ghost-pos)
                                       #t))))))
  (ui/render-in-game game))

;; Macro used to make keypress actions within the game easily translatable to
;; gameplay actions
(define-macro (action input sym)
  `(if (and (eq? ,input key)
            (eq? pressed key-pressed))
       (game/action! game (quote ,sym))))

(define move-left-key    (char->integer #\A))
(define move-down-key    (char->integer #\S))
(define move-right-key   (char->integer #\D))
(define rotate-left-key  (char->integer #\Q))
(define rotate-right-key (char->integer #\E))
(define drop-piece-key   (char->integer #\W))

(define ingame-state
  (make-gamestate
   ; Update
   (lambda (delta)
     (game/update-drop-time! game delta)
     (if (game/drop-piece? game)
         (game/drop-piece! game)))

   ; Render
   (lambda (delta) (render-game game))
   
   ; Keypress
   (lambda (key pressed)
     (action move-left-key    move-left)
     (action move-down-key    move-down)
     (action move-right-key   move-right)
     (action rotate-left-key  rotate-left)
     (action rotate-right-key rotate-right)
     (action drop-piece-key   drop-piece)
     (if (and (eq? pressed key-pressed) (eq? key key-esc))
         (gamestate-switch exit-to-menu-state)))
   
   ; on-switch-to
   (lambda (prev)
     '())))

;;;
;;; C Hooks
;;;
(c-define (init-game-hook) () void 
          "scheme_init_game" "extern"
          (ui/init)
          (highscore/init)
          (let ((tex (load-texture "data/blocks.png")))
            (set! block-sprites (make-sprite-sheet tex 16 16)))
          (set! clear-row-sound (make-sound "data/row-clear.wav"))
          (set! place-piece-sound (make-sound "data/place-piece.wav")))

(c-define (update-game-hook delta) (long) bool
          "scheme_update_game" "extern"
          (if game-is-running
              ((gamestate-update current-gamestate) delta))
          game-is-running)

(c-define (render-game-hook delta) (long) void 
          "scheme_render_game" "extern"
          ((gamestate-render current-gamestate) delta))

(c-define (keyboard-input-callback key pressed) (int int) void 
          "scheme_keyboard_callback" "extern"
          ((gamestate-keypress current-gamestate) key pressed))

