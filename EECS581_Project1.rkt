#|
------------------Prologue--------------------
Program Name: EECS581_Project1
Description: Racket based Battleship game using the R-Cade game engine. Players can play against a random guessing AI or PvP Pass and Play. 

Reqiruments: Dr.Racket IDE (https://racket-lang.org)
Libraries: R-Cade (https://r-cade.io)
R-Cade Documentation: https://docs.racket-lang.org/r-cade/index.html

Current Bug: After implmenting 2-Player, the Player vs. AI gameplay is not working properly. It swaps
the boards like it's a pass and play game. In the push history, the "Game Over" push has a
function Player vs. AI implementation.

Input: Mouse and Key input in order to manage/play the game
Output:
	- Displays a 800x900 window with the running battleship game. It will always start at the home state

Author: Saje Cowell
Date Created: 8/28/24
--------------------------------------------
|#


#lang racket
(require r-cade) ; Import r-cade library for game development
(require racket/match) ; Import match library for matching

;;-------------Initialization---------------;;

;; Define States
(define home 0) ; Main Menu State
(define game-mode-selection 1) ; State to select Player vs Player or Player vs AI
(define ship-selection 2) ; State to select the number of ships
(define ship-placement 3) ; State to place ships on the board
(define in-play 4) ; Game is running
(define game-over 5) ; Game over state, if all ships are hit

;; Declare Variables
(define boardSize 10) ; Dimension of the board (10x10 grid)
(define cellSize 40) ; Pixel size of individual cells on the board
(define x-offset 200) ; X offset for drawing the grid
(define y-offset 200) ; Y offset for drawing the grid
(define button-width 160) ; Width of buttons in pixels
(define button-height 60) ; Height of buttons in pixels
(define currentState home) ; Current state of the game, starting at the main menu
(define num-ships 0) ; Number of ships chosen by the player
(define ships-placed 0) ; Number of ships placed on the board
(define opponent-y-offset 50) ; Y offset for the opponent's grid
(define player-y-offset 465) ; Y offset for the player's grid
(define playerTurn 0) ; 1 for Player 1's turn, 2 for Player 2's turn
(define game-mode '2-player) ; Default game mode is 2-player
(define player1-ships-placed 0) ; Number of ships placed by Player 1
(define player2-ships-placed 0) ; Number of ships placed by Player 2
(define current-player 1) ; Tracks the current player (1 or 2)

;; Track ship sizes and placements
(define ship-sizes '()) ; List of ship sizes
(define ships-placed-locations '()) ; List of ship locations on the board
(define ship-orientation 'horizontal) ; Default ship orientation is horizontal

;; Initialize a board with vectors using a given size
(define (createBoard size)
  (let ([board (make-vector size)]) ; Create a vector to represent the board
    (for ([i (in-range size)]) ; Loop through each row of the board
      (vector-set! board i (make-vector size #f))) ; Initialize each row with false values (no ship)
    board)) ; Return the created board

; Create boards/grids using the createBoard function
(define initialBoard (createBoard boardSize)) ; Board used during ship selection
(define opponentBoard (createBoard boardSize)) ; Board for the opponent (Player 2 or AI)
(define player1-board (createBoard boardSize)) ; Board for Player 1
(define player2-board (createBoard boardSize)) ; Board for Player 2 

;; Function to draw the grid for in-play state
(define (draw-grid x-offset y-offset board showShips)
  ;; Draw grid lines and filled cells
  (for ([i (in-range boardSize)]) ; Loop through rows
    (for ([j (in-range boardSize)]) ; Loop through columns
      ;; Draw grid lines
      (color 7) ; Set color to white for grid lines
      (rect (+ x-offset (* j cellSize)) ; Calculate X position
            (+ y-offset (* i cellSize)) ; Calculate Y position
            cellSize cellSize ; Width and height of the cell
            #:fill #f) ; Draw the cell outline (not filled)
      ;; Handle cell state: 'hit', 'miss', or ship present
      (let ([cell (vector-ref (vector-ref board i) j)]) ; Get the state of the cell
        (cond
          [(eq? cell 'hit) ; If cell is marked as 'hit'
           (color 8) ; Set color to red for hit
           (rect (+ x-offset (* j cellSize)) 
                 (+ y-offset (* i cellSize)) 
                 cellSize cellSize 
                 #:fill #t)] ; Draw a filled red cell
          [(eq? cell 'miss) ; If cell is marked as 'miss'
           (color 12) ; Set color to blue for miss
           (rect (+ x-offset (* j cellSize)) 
                 (+ y-offset (* i cellSize)) 
                 cellSize cellSize 
                 #:fill #t)] ; Draw a filled blue cell
          ;; Only show ships if showShips is true (i.e., on the player's own board)
          [(and (eq? cell #t) showShips) ; If cell contains a ship and showShips is true
           (color 7) ; Set color to white for player's ships
           (rect (+ x-offset (* j cellSize)) 
                 (+ y-offset (* i cellSize)) 
                 cellSize cellSize 
                 #:fill #t)]))))) ; Draw a filled white cell

;; Checks if the mouse click is within a given area
(define (mouse-in? mx my x y width height)
  (and (<= x mx (+ x width)) ; Check if mouse X is within bounds
       (<= y my (+ y height)))) ; Check if mouse Y is within bounds

;; Converts mouse position to board coordinates, given the correct offset
(define (mouse-to-board mx my x-offset y-offset)
  (let* ((adjusted-y-offset (- y-offset 20)) ; Adjust Y offset
         (col (quotient (- mx x-offset) cellSize)) ; Calculate column index
         (row (quotient (- my adjusted-y-offset) cellSize))) ; Calculate row index
    (if (and (>= col 0) (< col boardSize) (>= row 0) (< row boardSize)) ; Check if within bounds
        (cons row col) ; Return row and column as a pair
        #f))) ; Return false if out of bounds

;; Checks if a ship can be placed without overlapping or out of bounds
(define (can-place-ship? board row col size orientation)
  (let ([result (cond
                  [(eq? orientation 'horizontal) ; If orientation is horizontal
                   (and (<= (+ col size) boardSize) ; Ensure it fits horizontally
                        (for/and ([i (in-range size)]) ; Check each cell
                          (not (vector-ref (vector-ref board row) (+ col i)))))] ; Ensure cells are empty
                  [(eq? orientation 'vertical) ; If orientation is vertical
                   (and (<= (+ row size) boardSize) ; Ensure it fits vertically
                        (for/and ([i (in-range size)]) ; Check each cell
                          (not (vector-ref (vector-ref board (+ row i)) col))))])]) ; Ensure cells are empty
    (displayln (format "Checking placement at row ~a, col ~a, size ~a, orientation ~a: ~a"
                       row col size orientation result)) ; Debug output
    result)) ; Return true if placement is possible, false otherwise

;; Places a ship on the board
(define (place-ship board row col size orientation)
  (for ([i (in-range size)]) ; Loop through the size of the ship
    (if (eq? orientation 'horizontal) ; If orientation is horizontal
        (begin
          (vector-set! (vector-ref board row) (+ col i) #t)) ; Mark cells horizontally
        (begin
          (vector-set! (vector-ref board (+ row i)) col #t)))) ; Mark cells vertically
  ;; Add the ship's information to the ships-placed-locations
  (set! ships-placed-locations (cons (list row col size orientation) ships-placed-locations)))

;; Places ships for the opponent (AI or Player 2)
(define (place-opponent-ships ship-sizes)
  ;; Loop over the sizes of ships
  (for ([size ship-sizes])
    (let loop ()
      (let* ([row (random boardSize)] ; Random row
             [col (random boardSize)] ; Random column
             [orientation (if (zero? (random 2)) 'horizontal 'vertical)]) ; Random orientation
        (if (can-place-ship? opponentBoard row col size orientation) ; Check if placement is possible
            (begin
              (place-ship opponentBoard row col size orientation) ; Place the ship
              (printf "Placed opponent ship of size ~a at row ~a, col ~a, orientation ~a~n" size row col orientation)) ; Debug output
            (loop)))))) ; Retry if placement is invalid

;; Removes the most recently added ship from the board
(define (remove-ship board ship)
  (let* ((row (first ship)) ; Get row of the ship
         (col (second ship)) ; Get column of the ship
         (size (third ship)) ; Get size of the ship
         (orientation (fourth ship))) ; Get orientation of the ship
    (for ([i (in-range size)]) ; Loop through the size of the ship
      (if (eq? orientation 'horizontal) ; If orientation is horizontal
          (vector-set! (vector-ref board row) (+ col i) #f) ; Clear cells horizontally
          (vector-set! (vector-ref board (+ row i)) col #f)))) ; Clear cells vertically
  (set! ships-placed-locations (rest ships-placed-locations))) ; Remove the ship from the list

;; 50/50 Random Number Generator to determine who starts the game first
(define (coinToss)
  (cond [(eq? game-mode '2-player) ; If game mode is 2-player
         (set! playerTurn (if (zero? (random 2)) 1 2))] ; Randomly select Player 1 or Player 2
        [(eq? game-mode '1-player-vs-ai) ; If game mode is Player vs AI
         (set! playerTurn (if (zero? (random 2)) 1 0))])) ; Randomly select Player or AI

;; Function to print the board to the console (for debugging)
(define (print-board board)
  (for ([row (in-vector board)]) ; Loop through each row
    (printf "~a~n" row))) ; Print each row

;; Function to handle player's guess on the opponent's board
(define (player-guess board mouseX mouseY)
  (let* ((board-pos (mouse-to-board mouseX mouseY x-offset opponent-y-offset))) ; Convert mouse to board coordinates
    (when board-pos ; If the click is within the board
      (let ((row (car board-pos)) ; Get row from board position
            (col (cdr board-pos))) ; Get column from board position
        ;; Check if the cell is already guessed
        (cond
          [(or (eq? (vector-ref (vector-ref board row) col) 'hit) ; If cell is 'hit'
               (eq? (vector-ref (vector-ref board row) col) 'miss)) ; Or if cell is 'miss'
           (printf "Cell at ~a, ~a was already guessed. Try again!~n" row col)] ; Output message
          ;; If not guessed, proceed with hit or miss logic
          [(eq? (vector-ref (vector-ref board row) col) #t) ; If cell contains a ship
           (vector-set! (vector-ref board row) col 'hit) ; Mark cell as 'hit'
           (printf "Player hit at ~a, ~a!~n" row col)] ; Output hit message
          [else ; If cell is empty
           (vector-set! (vector-ref board row) col 'miss) ; Mark cell as 'miss'
           (printf "Player missed at ~a, ~a.~n" row col)]))))) ; Output miss message

;; Function for the opponent's guess (random for AI)
(define (opponent-guess board)
  (let loop ()
    (let* ((row (random boardSize)) ; Random row
           (col (random boardSize))) ; Random column
      ;; Check if the AI guessed an already guessed location
      (if (or (eq? (vector-ref (vector-ref board row) col) 'hit) ; If cell is 'hit'
              (eq? (vector-ref (vector-ref board row) col) 'miss)) ; Or if cell is 'miss'
          (loop) ; Retry if it was already guessed
          ;; Otherwise, handle the guess
          (cond
            [(eq? (vector-ref (vector-ref board row) col) #t) ; If cell contains a ship
             (vector-set! (vector-ref board row) col 'hit) ; Mark cell as 'hit'
             (printf "Opponent hit at ~a, ~a!~n" row col)] ; Output hit message
            [else ; If cell is empty
             (vector-set! (vector-ref board row) col 'miss) ; Mark cell as 'miss'
             (printf "Opponent missed at ~a, ~a.~n" row col)]))))) ; Output miss message

;; Function to check if the game is over (all ships are hit)
(define (check-game-over board)
  (let ([ship-part-count 0] ; Total ship parts (all cells with #t)
        [hit-part-count 0]) ; Total hit parts (all cells with 'hit')
    ;; Iterate over the board and count ship parts and hit parts
    (for ([row (in-vector board)]) ; Loop through each row
      (for ([cell (in-vector row)]) ; Loop through each cell
        (when (eq? cell #t) ; If cell contains a ship part
          (set! ship-part-count (+ ship-part-count 1))) ; Increment ship-part-count
        (when (eq? cell 'hit) ; If cell is hit
          (set! hit-part-count (+ hit-part-count 1))))) ; Increment hit-part-count
    ;; Debug output to check counts
    (printf "Ship parts: ~a, Hit parts: ~a~n" ship-part-count hit-part-count)
    ;; The game is over if all ship parts have been hit
    (and (= ship-part-count 0) (< ship-part-count hit-part-count)))) ; Return true if game is over

;; Game update function
(define (update state)
  (let* ((mouseX (mouse-x)) ; Get mouse X position
         (mouseY (mouse-y)) ; Get mouse Y position
         (mouseClicked (btn-mouse))) ; Check if mouse button is clicked
    (cond
      ;; Home state - transition to game mode selection
      [(and (eq? currentState home)
            (mouse-in? mouseX mouseY 340 225 button-width button-height) ; Check if Start Game button is clicked
            mouseClicked)
       (set! currentState game-mode-selection)] ; Move to game mode selection state

      ;; Game Mode Selection State
      [(eq? currentState game-mode-selection)
       ;; Player vs AI Button
       (when (and (mouse-in? mouseX mouseY 300 200 button-width button-height) ; Check if Player vs AI button is clicked
                  mouseClicked)
         (set! game-mode '1-player-vs-ai) ; Set game mode to Player vs AI
         (set! currentState ship-selection)) ; Move to ship selection state
       ;; 2-Player Button
       (when (and (mouse-in? mouseX mouseY 300 300 button-width button-height) ; Check if 2-Player button is clicked
                  mouseClicked)
         (set! game-mode '2-player) ; Set game mode to 2-Player
         (set! currentState ship-selection))] ; Move to ship selection state

      ;; Ship Selection State
      [(eq? currentState ship-selection)
       (for ([i (in-range 5)]) ; Loop through options for number of ships (1 to 5)
         (let ((option-y (+ 60 (* i 50)))) ; Calculate Y position for each option
           (when (and (mouse-in? mouseX mouseY 350 option-y 100 40) ; Check if an option is clicked
                      mouseClicked)
             (set! currentState ship-placement) ; Move to ship placement state
             (set! num-ships (+ i 1)) ; Set number of ships based on selection
             (set! ship-sizes (reverse (build-list num-ships add1))))))] ; Create ship sizes list (1 to num-ships)

      ;; Ship Placement State
      [(eq? currentState ship-placement)
       ;; Toggle orientation on LEFT arrow key press
       (when (btn-left)
         (set! ship-orientation (if (eq? ship-orientation 'horizontal) 'vertical 'horizontal))) ; Toggle orientation

       ;; Determine current player's board and placement status
       (define current-board (if (= current-player 1) player1-board player2-board)) ; Select current player's board
       ;; Use a getter function to obtain the number of ships placed for the current player
       (define get-ships-placed (if (= current-player 1) player1-ships-placed player2-ships-placed)) ; Get ships placed
       (define set-ships-placed (if (= current-player 1) ; Set ships placed
                                    (lambda (val) (set! player1-ships-placed val))
                                    (lambda (val) (set! player2-ships-placed val))))

       ;; Adjusted y-offset for ship placement grid
       (let* ((adjusted-y-offset (+ y-offset 20))) ; Adjust y-offset for grid
         ;; Place ships
         (when (and mouseClicked (< get-ships-placed num-ships)) ; If not all ships are placed and mouse is clicked
           (let* ((board-pos (mouse-to-board mouseX mouseY x-offset adjusted-y-offset)) ; Convert mouse to board coordinates
                  (current-ship-size (list-ref ship-sizes get-ships-placed))) ; Get current ship size
             (when (and board-pos
                        (can-place-ship? current-board (car board-pos) (cdr board-pos) current-ship-size ship-orientation)) ; Check if ship can be placed
               (place-ship current-board (car board-pos) (cdr board-pos) current-ship-size ship-orientation) ; Place ship
               (set-ships-placed (+ get-ships-placed 1)))))) ; Increment ships placed

       ;; Check if all ships are placed
       (when (= get-ships-placed num-ships) ; If all ships are placed
         ;; If player 1 finished placing ships, switch to player 2
         (if (and (= current-player 1) (eq? game-mode '2-player)) ; If player 1 and 2-player mode
             (begin
               (set! current-player 2) ; Switch to player 2
               ;; Clear the ships-placed-locations for the second player's placement
               (set! ships-placed-locations '()) ; Clear list for player 2
               (set! currentState ship-placement)) ; Continue placing for player 2
             ;; Else, if we are done with player 2's placement, move to the in-play state
             (begin
               ;; Start Game Button
               (when (and (mouse-in? mouseX mouseY 300 750 button-width button-height) mouseClicked) ; If start button is clicked
                 (if (eq? game-mode '2-player) ; If 2-player mode
                     (begin
                       (set! currentState in-play) ; Move to in-play state
                       (coinToss)) ; Decide who goes first
                     (begin
                       ;; For 1-player vs AI
                       (place-opponent-ships ship-sizes) ; AI places ships
                       (set! currentState in-play) ; Move to in-play state
                       (coinToss)))))))

       ;; Revert Button
       (when (and (mouse-in? mouseX mouseY 300 650 button-width button-height) mouseClicked ; If revert button is clicked
                  (> get-ships-placed 0)) ; If at least one ship is placed
         (remove-ship current-board (first ships-placed-locations)) ; Remove last placed ship
         (set-ships-placed (- get-ships-placed 1)))] ; Decrease ships placed count

      ;; In-Play State
      [(eq? currentState in-play)
       ;; Display whose turn it is
       (font wide-font) ; Set font size
       (text 20 20 (if (= playerTurn 1) "Player 1's Turn" "Player 2's Turn")) ; Display turn

       ;; If it's Player 1's turn
       (if (= playerTurn 1)
           (begin
             ;; Player 1 guesses on Player 2's board (top grid)
             (text 330 10 "Player 2's Board") ; Label for Player 2's board
             (draw-grid x-offset opponent-y-offset player2-board #f) ; Draw Player 2's board without showing ships
             ;; Player 1's own board (bottom grid)
             (text 355 450 "Your Board") ; Label for Player 1's board
             (draw-grid x-offset player-y-offset player1-board #t) ; Draw Player 1's board showing ships

             ;; Handle clicks for Player 1's turn
             (when mouseClicked
               (let ((board-pos (mouse-to-board mouseX mouseY x-offset opponent-y-offset))) ; Convert mouse to board coordinates
                 (when board-pos ; If the click is within the board
                   ;; Ensure Player 1 interacts with Player 2's board (top grid)
                   (if (or (eq? (vector-ref (vector-ref player2-board (car board-pos)) (cdr board-pos)) 'hit)
                           (eq? (vector-ref (vector-ref player2-board (car board-pos)) (cdr board-pos)) 'miss)) ; If cell was already guessed
                       (printf "Cell was already guessed. Try a different one!~n") ; Output message
                       (begin
                         (player-guess player2-board mouseX mouseY) ; Player 1 guesses on Player 2's board
                         ;; Check if Player 1 has won
                         (when (check-game-over player2-board) ; If Player 1 wins
                           (printf "Player 1 wins! All Player 2's ships are hit!~n") ; Output win message
                           (set! currentState game-over)) ; Set game state to game over
                         ;; Switch to Player 2's turn
                         (set! playerTurn 2))))))) ; Switch turn to Player 2

           ;; If it's Player 2's turn
           (begin
             ;; Player 2 guesses on Player 1's board (top grid)
             (text 330 10 "Player 1's Board") ; Label for Player 1's board
             (draw-grid x-offset opponent-y-offset player1-board #f) ; Draw Player 1's board without showing ships
             ;; Player 2's own board (bottom grid)
             (text 355 450 "Your Board") ; Label for Player 2's board
             (draw-grid x-offset player-y-offset player2-board #t) ; Draw Player 2's board showing ships

             ;; Handle clicks for Player 2's turn
             (when mouseClicked
               (let ((board-pos (mouse-to-board mouseX mouseY x-offset opponent-y-offset))) ; Convert mouse to board coordinates
                 (when board-pos ; If the click is within the board
                   ;; Ensure Player 2 interacts with Player 1's board (top grid)
                   (if (or (eq? (vector-ref (vector-ref player1-board (car board-pos)) (cdr board-pos)) 'hit)
                           (eq? (vector-ref (vector-ref player1-board (car board-pos)) (cdr board-pos)) 'miss)) ; If cell was already guessed
                       (printf "Cell was already guessed. Try a different one!~n") ; Output message
                       (begin
                         (player-guess player1-board mouseX mouseY) ; Player 2 guesses on Player 1's board
                         ;; Check if Player 2 has won
                         (when (check-game-over player1-board) ; If Player 2 wins
                           (printf "Player 2 wins! All Player 1's ships are hit!~n") ; Output win message
                           (set! currentState game-over)) ; Set game state to game over
                         ;; Switch back to Player 1's turn
                         (set! playerTurn 1))))))))])))

;; Function to approximate text centering horizontally
(define (center-text x y width text-str)
  (let* ((font-width (font-advance)) ; Get the width of the font
         (text-length (* (string-length text-str) font-width)) ; Calculate the length of the text
         (text-x (+ x (/ (- width text-length) 2)))) ; Calculate the starting X position to center the text
    (text text-x y text-str))) ; Draw the text at the centered position

;; Function to Draw the State
(define (draw state)
  (begin
    (cls) ; Clear the screen
    (cond
      ;; Draw Home Menu
      [(eq? currentState home)
       (color 7) ; Set color to white for the button
       (rect 300 200 button-width button-height #:fill #t) ; Draw Start Game button
       (color 0) ; Set color to black for the text
       (text 340 225 "Start Game") ; Draw text on the button

       ;; Set the font to a larger size
       (font wide-font)
       (color 7) ; Set color to white
       (text 300 100 "Welcome to Battleship!")] ; Draw welcome message

      ;; Draw Game Mode Selection
      [(eq? currentState game-mode-selection)
       (color 7) ; Set color to white
       (font wide-font) ; Set font size
       (text 315 100 "Select Game Mode") ; Draw header text
       ;; Draw Player vs A.I. button
       (rect 300 200 button-width button-height #:fill #t) ; Draw button
       (color 0) ; Set color to black for text
       (center-text 300 225 button-width "1-Player vs A.I.") ; Draw centered text on button
       ;; Draw 2-Player button
       (color 7) ; Set color to white
       (rect 300 300 button-width button-height #:fill #t) ; Draw button
       (color 0) ; Set color to black for text
       (center-text 300 325 button-width "2-Player")] ; Draw centered text on button

      ;; Draw Ship Selection Screen
      [(eq? currentState ship-selection)
       (color 0) ; Set color to black for text
       (font tall-font) ; Set font size
       (text 20 20 "Select the number of ships:") ; Draw header text
       (for ([i (in-range 5)]) ; Loop through ship options (1 to 5)
         (let ((option-y (+ 60 (* i 50)))) ; Calculate Y position for each option
           (color 7) ; Set color to white for button
           (rect 350 option-y 100 40 #:fill #t) ; Draw button
           (color 0) ; Set color to black for text
           ;; Draw centered text for each ship option
           (text 375 (+ option-y 20) (format "~a Ships" (+ i 1)))))
       (text 20 300 "Click on a number to select the number of ships.")] ; Draw instructions

      ;; Draw Ship Placement State
      [(eq? currentState ship-placement)
       (color 7) ; Set color to white
       (font wide-font) ; Set font size
       (text 175 50 (format "Player ~a, Place Your Ships! Press LEFT arrow to toggle orientation." current-player)) ; Draw header text

       ;; Determine which board to draw for the current player
       (define current-board (if (= current-player 1) player1-board player2-board)) ; Select current player's board

       ;; Draw the grid
       (draw-grid x-offset y-offset current-board #t) ; Draw grid showing ships

       ;; Draw Start Game button after all ships placed for the current player
       (when (= (if (= current-player 1) player1-ships-placed player2-ships-placed) num-ships) ; If all ships placed
         (color 7) ; Set color to white
         (rect 300 750 button-width button-height #:fill #t) ; Draw Start Game button
         (color 0) ; Set color to black for text
         (center-text 300 775 button-width "Start Game")) ; Draw centered text on button

       ;; Draw Revert button
       (color 7) ; Set color to white
       (rect 300 650 button-width button-height #:fill #t) ; Draw Revert button
       (color 0) ; Set color to black for text
       (center-text 300 675 button-width "Revert Ship")] ; Draw centered text on button

      ;; Draw In-Play State
      [(eq? currentState in-play)
       (font wide-font) ; Set font size
       (text 20 20 (if (= playerTurn 1) "Player 1's Turn" "Player 2's Turn")) ; Display current player's turn
       
       ;; Draw the boards based on the current turn
       (if (= playerTurn 1)
           (begin
             ;; Player 1's turn: show Player 2's board on top, Player 1's board on bottom
             (text 330 10 "Player 2's Board") ; Label for Player 2's board
             (draw-grid x-offset 25 player2-board #f) ; Draw Player 2's board without showing ships
             (text 355 450 "Your Board") ; Label for Player 1's board
             (draw-grid x-offset player-y-offset player1-board #t)) ; Draw Player 1's board showing ships
           (begin
             ;; Player 2's turn: show Player 1's board on top, Player 2's board on bottom
             (text 330 10 "Player 1's Board") ; Label for Player 1's board
             (draw-grid x-offset 25 player1-board #f) ; Draw Player 1's board without showing ships
             (text 355 450 "Your Board") ; Label for Player 2's board
             (draw-grid x-offset player-y-offset player2-board #t)))] ; Draw Player 2's board showing ships

      ;; Draw Game Over State
      [(eq? currentState game-over)
       (font wide-font) ; Set font size
       (color 7) ; Set color to white
       (text 300 100 "Game Over!")]))) ; Draw Game Over message

;;-------------Run Game---------------;;
;; Game loop function calls both update and draw state each frame
(define (game-loop)
  (begin
    (update currentState) ; Update the game state
    (draw currentState))) ; Draw the current game state

;; Start the game loop
(run game-loop
     800 ; Width of the window
     900 ; Height of the window
     #:fps 60) ; Set the frame rate to 60 FPS
