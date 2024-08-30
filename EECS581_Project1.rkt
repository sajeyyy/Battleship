#lang racket
(require r-cade)
(require racket/match)

;; Define States
(define home 0)
(define ship-selection 1)
(define playing 2)

;; Declare Variables
(define boardSize 10)
(define cellSize 40)
(define x-offset 200)
(define y-offset 200)
(define button-width 160)
(define button-height 60)
(define currentState home)
(define num-ships 0)
(define char-width 10)

;; Create the Board
(define (createBoard size)
  (make-vector size (make-vector size #f)))

(define initialBoard (createBoard boardSize))

;; Checks if the mouse click is within a given area
(define (mouse-in? mx my x y width height)
  (and (<= x mx (+ x width))
       (<= y my (+ y height))))

;; Game update function
(define (update state)
  (let* ((mouseX (mouse-x))
         (mouseY (mouse-y))
         (mouseClicked (btn-mouse)))
    (cond
      [(and (eq? currentState home)
            (mouse-in? mouseX mouseY 340 225 button-width button-height)
            mouseClicked)
       (set! currentState ship-selection)
       (printf "Transitioning to Ship Selection State~n")]

      [(eq? currentState ship-selection)
       (for ([i (in-range 5)])
         (let ((option-y (+ 60 (* i 50))))
           (when (and (mouse-in? mouseX mouseY 375 option-y 100 40)
                      mouseClicked)
             (set! currentState playing)
             (set! num-ships (+ i 1))
             (printf "Transitioning to Playing State with ~a ships~n" num-ships))))])))

;; Function to approximate text centering horizontally
(define (center-text x y width text-str)
  (let* ((font-width (font-advance))
         (text-length (* (string-length text-str) font-width))
         (text-x (+ x (/ (- width text-length) 2))))
    (text text-x y text-str)))

;; Function to Draw the State
(define (draw state)
  (begin
    (cls)  ; Clear the screen
    (cond
      ;; Draw Home Menu
      [(eq? currentState home)
       (color 7)  ; Set color to white using palette index
       (rect 300 200 button-width button-height #:fill #t)
       (color 0)  ; Set color to black using palette index
       (text 340 225 "Start Game")

       ;; Set the font to a larger size
       (font wide-font)
       (color 7)
       (text 300 100 "Welcome to Battleship!")]

      ;; Draw Ship Selection Screen
      [(eq? currentState ship-selection)
       (color 0)  ; Set color to black for text
       
       ;; Set the font to a medium size
       (font tall-font)
       (text 20 20 "Select the number of ships:")
       (for ([i (in-range 5)])
         (let ((option-y (+ 60 (* i 50))))
           (color 7)  ; Set color to white for button
           (rect 350 option-y 100 40 #:fill #t)
           (color 0)  ; Set color to black for text
           ;; Correctly use center-text to draw the text
           (text 375 (+ option-y 20) (format "~a Ships" (+ i 1)))))
       (text 20 300 "Click on a number to select the number of ships.")]

      ;; Draw Playing State
      [(eq? currentState playing)
       (color 7)
       (font wide-font)
       (text 340 100 "Place Your Ships!")
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
    (update currentState)
    (draw currentState)))

;; Start the game loop
(run game-loop
     800    ; width of the window
     800    ; height of the window
     #:fps 60)  ; Set the frame rate to 60 FPS
