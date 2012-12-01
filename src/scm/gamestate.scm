;;;
;;; Gamestate
;;;
(define-structure gamestate
  ; These are all lambdas used when functioning within a gamestate.
  update
  render
  keypress

  ; Called when this state is switched to
  on-switch-to)

;; Holds our current gamestate
(define current-gamestate '())

;; Function used to switch between gamestates
(define (gamestate-switch new-state)
  (let ((prev current-gamestate))
    (set! current-gamestate new-state)
    ((gamestate-on-switch-to new-state) prev)))
