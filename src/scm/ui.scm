;;;;
;;;; ui.scm
;;;;
;;;; In charge of rendering the user interface to the screen.
;;;;

;; constants
(define ui/x-off 224)
(define ui/level-text-position (list ui/x-off 128))
(define ui/score-text-position (list ui/x-off 144))
(define ui/lines-text-position (list ui/x-off 160))
(define ui/characters-wide 7)

;; sounds
(define menu-move-sound #f)
(define menu-select-sound #f)

(define (make-sprites-list sheet)
  (map (lambda (x) 
         (get-sprite sheet x 0))
       (iota 6)))

;; Renders a board background which is centered.  Width and height are in
;; blocks.  Returns a list with the x/y origin
(define (render-centered-board-background width height)
  (let* ((x-origin (quotient (- (gfx/get-width) 
                                (* 16 width)) 2))
         (y-origin (quotient (- (gfx/get-height)
                                (* 16 height)) 2)))
    (render-board-background (get-sprite block-sprites 0 0)
                             x-origin y-origin
                             width
                             height)
    (list x-origin y-origin)))

;; data
(define ui-font '())

(define (ui/init)
  (set! ui-font (make-font (load-texture "data/font.png") 16 16))
  (set! menu-move-sound (make-sound "data/menu-move.wav"))
  (set! menu-select-sound (make-sound "data/menu-select.wav")))

(define (ui/render-string pos text . color)
  (render-string ui-font
                 (car pos) (cadr pos)
                 text
                 color: (if (not (null? color))
                            (car color)
                            '(255 255 255 255))))

(define (ui/render-in-game game)
  (ui/render-string (list ui/x-off 16) "Next")
  (ui/render-string ui/level-text-position
                    (str "Level: " 
                         (list 'pad-right (block-game-level game) 
                               ui/characters-wide
                               #\space)))
  (ui/render-string ui/score-text-position
                    (str "Score: " (list 'pad-right (block-game-score game) 
                                         ui/characters-wide
                                         #\space)))
  (ui/render-string ui/lines-text-position
                    (str "Lines: " (list 'pad-right (block-game-lines game)
                                         ui/characters-wide
                                         #\space))))

;;;
;;; Highscore UI
;;;

(define highscore-block-width 30)
(define highscore-block-height 16)
(define highscore-string-width 9)

(define highscore-state
  (make-gamestate
   ; update
   (lambda (dt) '())
   
   ; render
   (lambda (delta)
     (let* ((x-y-origin (render-centered-board-background 
                         highscore-block-width
                         highscore-block-height))
            (x-origin (car x-y-origin))
            (y-origin (cadr x-y-origin))
            (score-start-y (+ y-origin 48)))
       
       ; Draw the text for Highscore
       (ui/render-string 
        (list (+ x-origin (* (quotient (- highscore-block-width
                                          highscore-string-width)
                                       2) 16)) 
              (+ y-origin 16))
        "Highscore")
       
       ; Render individual scores
       (for-each 
        (lambda (score count)
          (let ((name-length (string-length (car score)))
                (digit-count (length (integer->list (cadr score)))))
            (ui/render-string (list (+ x-origin 16)
                                    (+ y-origin 48 (* count 16)))
                              (str (car score)
                                   (list->string
                                    (make-list (- highscore-block-width
                                                  2
                                                  name-length
                                                  digit-count)
                                               #\.))
                                   (cadr score)))))
        highscores (iota 10))
       
       (ui/render-string
        (list (+ x-origin 
                 (* (quotient (- highscore-block-width
                                 (string-length "Press any key to return"))
                              2) 16))
              (+ y-origin (* 16 (- highscore-block-height 2))))
        "Press any key to return")))
   
   ; keypress
   (lambda (key pressed)
     (if (eq? pressed key-pressed)
         (gamestate-switch mainmenu-state)))

   ; on-switch-to
   (lambda (prev) '())))

(define (new-highscore-state new-score)
  (let ((name  "")
        (score new-score))
    (make-gamestate
     ; update
     (lambda (delta) '())
   
     ; render
     (lambda (delta) 
       (let* ((x-y-origin (render-centered-board-background 24 5))
              (x-origin   (car x-y-origin))
              (y-origin   (cadr x-y-origin)))
         (ui/render-string (list (+ x-origin 32) (+ y-origin 16))
                           "Enter your name.")
         (ui/render-string (list (+ x-origin 32) (+ y-origin 48))
                           (str "*" name))))

     ; keypress 
     (lambda (key pressed) 
       (if (eq? pressed key-pressed)
           (cond 
            ((and (>= key 32) (<= key 126) 
                  (< (string-length name) highscore/max-name-length))
             ; Check for shift if we need to lower/upper case
             (if (and (>= key 65) (<= key 90)
                      (eq? (glfw/get-key key-lshift) key-released)
                      (eq? (glfw/get-key key-rshift) key-released))
                 (set! name 
                       (string-append name (string (integer->char 
                                                    (+ key 32)))))
                 (set! name 
                       (string-append name (string (integer->char key))))))
            ((and (or (= key 127) (= key key-backspace))
                  (> (string-length name) 0))
             (set! name (substring name 0 (- (string-length name) 1))))
            ((eq? key key-enter)
             (begin
               (highscore/add! name score)
               (gamestate-switch highscore-state))))))

     ; on-switch-to
     (lambda (prev) '()))))

;;;
;;; Menus
;;;

(define (set-menu menu)
  (set! menu-data (list 0 menu)))

(define mainmenu-items
  (list "Main Menu"
        (list
         (list "New Game"
               (lambda ()
                 (set! game (new-game '(ghost)))
                 (gamestate-switch ingame-state)))
         (list "Highscores"
               (lambda ()
                 (gamestate-switch highscore-state)))
         (list "Exit"
               (lambda ()
                 (set! game-is-running #f))))))

(define menu-data (list 0 mainmenu-items))

(define (menu/get-selected-index) (car menu-data))
(define (menu/get-data) (cadr menu-data))

;; Renders the menu to the screen
(define (menu/render delta data)
  (let* ((items  (cadr (menu/get-data)))
         (select (menu/get-selected-index))
         (count  (length items))
         (widest (fold (lambda (n cur) (max cur (string-length (car n))))
                       0 items))
         (x-y-origin (render-centered-board-background (+ 2 widest)
                                                       (+ 4 count)))
         (x-origin (car x-y-origin))
         (y-origin (cadr x-y-origin)))

    ; Render the menu name
    (ui/render-string (list (+ x-origin (* 16 1)) (+ y-origin (* 16 1)))
                      (car (cadr data)))

    ; Render the menu items
    (for-each (lambda (item count)
                (let ((selected (eq? count select)))
                  (ui/render-string (list (+ x-origin (if selected 16 32))
                                          (+ y-origin (* count 16) 48))
                                    (if selected
                                        (string-append ">" (car item))
                                        (car item)))))
              items (iota count 0))))

(define mainmenu-state
  (make-gamestate
   ; update
   (lambda (delta)
     '())

   ; render
   (lambda (delta)
     (menu/render delta menu-data))

   ; keypress
   (lambda (key pressed)
     (if (eq? pressed key-pressed)
         (let ((current (menu/get-selected-index)))
           (cond
            ((or (eq? key key-down)
                 (eq? key move-down-key))
             (if (< current (- (length (cadr (menu/get-data))) 1))
                 (begin
                   (sound/play menu-move-sound)
                   (set! menu-data (list (+ 1 current) (menu/get-data))))))
            ((or (eq? key key-up)
                 (eq? key drop-piece-key))
             (if (> current 0)
                 (begin
                   (sound/play menu-move-sound)
                   (set! menu-data (list (- current 1) (menu/get-data))))))
            ((eq? key key-esc)
             (set! game-is-running #f))
            ((eq? key key-enter)
             (begin
               (sound/play menu-select-sound)
               ((cadr (list-ref (cadr (menu/get-data)) current)))))))))
   
   
   ; on-switch-to
   (lambda (prev)
     (set! menu-data (list 0 mainmenu-items)))))

(gamestate-switch mainmenu-state)

;;;
;;; Prompts
;;;
;;; Used to display messages and get possible input from the user.
;;;

(define (prompt/draw text)
  (let* ((x-y-origin (render-centered-board-background 
                      (+ 2 (string-length text)) 3))
         (x-origin (car x-y-origin))
         (y-origin (cadr x-y-origin)))
    (ui/render-string (list (+ x-origin 16) (+ y-origin 16)) text)))

(define exit-to-menu-state
  (make-gamestate
   ; update
   (lambda (dt) '())
   ; render
   (lambda (delta)
     ((gamestate-render ingame-state) delta)
     (prompt/draw "Exit to menu?(Y/N)"))
   ; keypress
   (lambda (key pressed)
     (if (eq? pressed key-pressed)
         (if (eq? (char->integer #\Y) key)
             (begin
               (set! game #f)
               (gamestate-switch mainmenu-state))
             (gamestate-switch ingame-state))))
   
   ; switch to
   (lambda (prev) '())))

(define (simple-prompt-state text return-state)
  (make-gamestate
   ; update
   (lambda (dt) '())
   ; render
   (lambda (delta)
     ((gamestate-render ingame-state) delta)
     (prompt/draw text))
   ; keypress
   (lambda (key pressed)
     (gamestate-switch return-state))
   
   ; switch to
   (lambda (prev) '())))

