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
(define currentState home) ; Start the current state at homescreen
(define num-ships 0) ; Number of ships selected

;; Create the Board
(define (createBoard size)
  (make-vector size (make-vector size #f)))  ; False means no part of a ship in that cell

(define initialBoard (createBoard boardSize))  ; Initialize the board

;; Checks if the mouse click is within a given area
(define (mouse-in? mx my x y width height)
  (and (<= x mx (+ x width))
       (<= y my (+ y height))))

;; Game update function
(define (update state)
  (let* ((mouseX (mouse-x))  ; Get mouse X position
         (mouseY (mouse-y))  ; Get mouse Y position
         (mouseClicked (btn-mouse)))  ; Check if the mouse button is pressed
    (cond
      ;; Transition from home to ship-selection
      [(and (eq? currentState home)
            (and (>= mouseX x-offset)
                 (<= mouseX (+ x-offset button-width))
                 (>= mouseY y-offset)
                 (<= mouseY (+ y-offset button-height))
                 mouseClicked))
       (set! currentState ship-selection)]  ; Transition to ship selection state

      ;; Handle ship selection
      [(eq? currentState ship-selection)
       ;; Check if mouse is clicked on any of the options
       (for ([i (in-range 5)])
         (let ((option-y (+ 60 (* i 50))))
           (when (and (>= mouseX 50)
                      (<= mouseX 150)
                      (>= mouseY option-y)
                      (<= mouseY (+ option-y 40))
                      mouseClicked)
             (set! currentState playing)  ; Set the state to playing
             (set! num-ships (+ i 1)))))]))) ; Store the selected number of ships

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
       ;; Draw options for selecting the number of ships
       (for ([i (in-range 5)])
         (let ((option-y (+ 60 (* i 50))))
           (rect 50 option-y 100 40 #:fill #t)  ; Draw a rectangle button for each option
           (text 60 (+ option-y 30) (format "Option ~a" (+ i 1)))))  ; Display option number

       ;; Draw additional instructions if necessary
       (text 20 300 "Click on a number to select the number of ships.")]
      
      ;; Draw Playing State
      [(eq? currentState playing)
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

;; Start the game loop
(run game-loop
     800    ; width of the window
     800    ; height of the window
     #:fps 60)  ; Set the frame rate to 60 FPS
