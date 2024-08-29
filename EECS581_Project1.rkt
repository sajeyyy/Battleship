#lang racket
(require r-cade)
(require racket/match)

;; Define States
(define home 0)
(define ship-selection 1)
(define playing 2)

;; Declare Variables
(define boardSize 10)  ; Size of the board
(define cellSize 40)  ; Size of each cell in the grid
(define x-offset 200)  ; Horizontal offset for grid positioning
(define y-offset 200)  ; Vertical offset for grid positioning
(define button-width 160)  ; Width of the start button
(define button-height 60)  ; Height of the start button

;; Create the Board
(define (createBoard size)
  (make-vector size (make-vector size #f)))  ; False means no part of the ship in that cell

(define initialBoard (createBoard boardSize))  ; Initialize the board

;; Game State
(define currentState home)  ; Start in the home menu

;; Checks if the mouse click is within a certain area
(define (mouse-in? mx my x y width height)
  (and (<= x mx (+ x width))
       (<= y my (+ y height))))

;; Function to Update the State
(define (update state)
  (let ([mx (mouse-x)] [my (mouse-y)])  ; Get mouse position
    (cond
      ;; Check if the mouse is detected and handle transitions based on mouse clicks
      [(and (eq? currentState home)
            (mouse-in? mx my x-offset y-offset button-width button-height)
            (btn-mouse))  ; Check if the left mouse button is pressed
       (begin
         (set! currentState ship-selection)  ; Go to ship selection screen
         (printf "Transitioning to ship-selection screen\n"))]  ; Debug output
      ;; Add more conditions here for other states
      [else state])))

;; Function to Draw the State
(define (draw state)
  (begin
    (cls)  ; Clear the screen
    (cond
      ;; Draw Home Menu
      [(eq? currentState home)
       (text (+ x-offset 20) (+ y-offset 20) "Welcome to Battleship!")
       ;; Draw button with color
       (rect x-offset y-offset button-width button-height #:fill #t)  ; Draw filled button
       (text (+ x-offset 20) (+ y-offset (+ button-height 20)) "Start Game")]
      
      ;; Draw Ship Selection Screen
      [(eq? currentState ship-selection)
       (text 20 20 "Select the number of ships:")
       ;; Add more code here to handle ship selection
       
       ;; Draw the grid
       (for ([i (in-range (+ boardSize 1))])
         ;; Vertical lines
         (line (+ x-offset (* i cellSize)) y-offset
               (+ x-offset (* i cellSize)) (+ y-offset (* boardSize cellSize)))
         ;; Horizontal lines
         (line x-offset (+ y-offset (* i cellSize))
               (+ x-offset (* boardSize cellSize)) (+ y-offset (* i cellSize))))]

      ;; Draw Playing State
      [(eq? currentState playing)
       ;; Add code here to draw the game board and other game elements
       (text 20 20 "Game in Progress")
       ;; Draw the grid
       (for ([i (in-range (+ boardSize 1))])
         ;; Vertical lines
         (line (+ x-offset (* i cellSize)) y-offset
               (+ x-offset (* i cellSize)) (+ y-offset (* boardSize cellSize)))
         ;; Horizontal lines
         (line x-offset (+ y-offset (* i cellSize))
               (+ x-offset (* boardSize cellSize)) (+ y-offset (* i cellSize))))])))

;; Game loop function that includes both update and draw
(define (game-loop)
  (begin
    (update currentState)  ; Call update with the current state
    (draw currentState)))  ; Call draw with the current state

;; Start the game loop with window dimensions and optional settings
(run game-loop
     800    ; width of the window
     800    ; height of the window
     #:fps 60)  ; Set the frame rate to 60 FPS
