#lang racket
(require r-cade)
(require racket/match)

;; Declare Variables
(define boardSize 10)  ; Size of the board
(define cellSize 40)  ; Size of each cell in the grid
(define x-offset 200)  ; Horizontal offset for grid positioning
(define y-offset 200)  ; Vertical offset for grid positioning

;; Create the Board
(define (createBoard size)
  (make-vector size (make-vector size #f)))  ; False means no ship part here

(define initialBoard (createBoard boardSize))  ; Initialize the board

;; Game update function
(define (update state)
  (let* ((mousePosition (btn-mouse))  ; Get mouse position within update function
         (new-state state))  ; Placeholder for state updates
    ;; Add logic here to update game state using mousePosition or other inputs
    new-state))

;; Game draw function
(define (draw state)
  (begin
    (cls)  ; Clear the screen

    ;; Draw the grid
    (for ([i (in-range (+ boardSize 1))])
      ;; Vertical lines
      (line (+ x-offset (* i cellSize)) y-offset
            (+ x-offset (* i cellSize)) (+ y-offset (* boardSize cellSize)))
      ;; Horizontal lines
      (line x-offset (+ y-offset (* i cellSize))
            (+ x-offset (* boardSize cellSize)) (+ y-offset (* i cellSize))))))

;; Game loop function that includes both update and draw
(define (GameLoop)
  (begin
    (update initialBoard)  ; Call update with the initial state
    (draw initialBoard)))  ; Call draw with the current state

;; Start the game loop with window dimensions and optional settings
(run GameLoop
     800    ; width of the window
     800    ; height of the window
     #:fps 60)  ; Set the frame rate to 60 FPS
