#lang racket
(require r-cade)
(require racket/match)

;;-------------Initialization---------------;;

;; Define States
(define home 0)
(define game-mode-selection 1)
(define ai-difficulty-selection 2) ; Unique state for AI difficulty selection
(define ship-selection 3)
(define ship-placement 4)
(define in-play 5)
(define game-over 6)

;; Declare Variables
(define boardSize 10)
(define cellSize 40)
(define x-offset 200)
(define y-offset 200)
(define button-width 160)
(define button-height 60)
(define currentState home)
(define num-ships 0)
(define ships-placed 0)
(define opponent-y-offset 50)
(define player-y-offset 465)
(define playerTurn 0)
(define game-mode '2-player)
(define ai-difficulty 'easy)
(define player1-ships-placed 0)
(define player2-ships-placed 0)
(define current-player 1)

;; Track ship sizes and placements
(define ship-sizes '())
(define ships-placed-locations '())
(define ship-orientation 'horizontal)

;; Variables for Medium AI
(define last-hit #f)
(define target-queue '())

;; Initialize a board with vectors using a given size
(define (createBoard size)
  (let ([board (make-vector size)])
    (for ([i (in-range size)])
      (vector-set! board i (make-vector size #f)))
    board))

(define initialBoard (createBoard boardSize))
(define opponentBoard (createBoard boardSize))
(define player1-board (createBoard boardSize))
(define player2-board (createBoard boardSize))

;; Function to draw the grid
(define (draw-grid x-offset y-offset board showShips)
  (for ([i (in-range boardSize)])
    (for ([j (in-range boardSize)])
      (color 7)
      (rect (+ x-offset (* j cellSize)) 
            (+ y-offset (* i cellSize)) 
            cellSize cellSize #:fill #f)
      (let ([cell (vector-ref (vector-ref board i) j)])
        (cond
          [(eq? cell 'hit)
           (color 8)
           (rect (+ x-offset (* j cellSize)) 
                 (+ y-offset (* i cellSize)) 
                 cellSize cellSize #:fill #t)]
          [(eq? cell 'miss)
           (color 12)
           (rect (+ x-offset (* j cellSize)) 
                 (+ y-offset (* i cellSize)) 
                 cellSize cellSize #:fill #t)]
          [(and (eq? cell #t) showShips)
           (color 7)
           (rect (+ x-offset (* j cellSize)) 
                 (+ y-offset (* i cellSize)) 
                 cellSize cellSize #:fill #t)])))))

;; Mouse click detection
(define (mouse-in? mx my x y width height)
  (and (<= x mx (+ x width)) (<= y my (+ y height))))

;; Convert mouse position to board coordinates
(define (mouse-to-board mx my x-offset y-offset)
  (let* ((adjusted-y-offset (- y-offset 20))
         (col (quotient (- mx x-offset) cellSize))
         (row (quotient (- my adjusted-y-offset) cellSize)))
    (if (and (>= col 0) (< col boardSize) (>= row 0) (< row boardSize))
        (cons row col)
        #f)))

;; Check if a ship can be placed without overlap or out of bounds
(define (can-place-ship? board row col size orientation)
  (let ([result (cond
                  [(eq? orientation 'horizontal)
                   (and (<= (+ col size) boardSize)
                        (for/and ([i (in-range size)])
                          (not (vector-ref (vector-ref board row) (+ col i)))))]
                  [(eq? orientation 'vertical)
                   (and (<= (+ row size) boardSize)
                        (for/and ([i (in-range size)])
                          (not (vector-ref (vector-ref board (+ row i)) col))))])])
    result))

;; Place ship on the board
(define (place-ship board row col size orientation)
  (for ([i (in-range size)])
    (if (eq? orientation 'horizontal)
        (vector-set! (vector-ref board row) (+ col i) #t)
        (vector-set! (vector-ref board (+ row i)) col #t)))
  (set! ships-placed-locations (cons (list row col size orientation) ships-placed-locations)))

;; Opponent ship placement (AI or Player 2)
(define (place-opponent-ships ship-sizes board)
  (for ([size ship-sizes])
    (let loop ()
      (let* ([row (random boardSize)]
             [col (random boardSize)]
             [orientation (if (zero? (random 2)) 'horizontal 'vertical)])
        (if (can-place-ship? board row col size orientation)
            (place-ship board row col size orientation)
            (loop))))))

;; Remove the last placed ship
(define (remove-ship board ship)
  (let* ((row (first ship))
         (col (second ship))
         (size (third ship))
         (orientation (fourth ship)))
    (for ([i (in-range size)])
      (if (eq? orientation 'horizontal)
          (vector-set! (vector-ref board row) (+ col i) #f)
          (vector-set! (vector-ref board (+ row i)) col #f)))
    (set! ships-placed-locations (rest ships-placed-locations))))

;; Coin toss to determine who starts
(define (coinToss)
  (cond
    [(eq? game-mode '2-player)
     (set! playerTurn (if (zero? (random 2)) 1 2))]
    [(eq? game-mode '1-player-vs-ai)
     (set! playerTurn (if (zero? (random 2)) 1 0))]))

;; Print the board for debugging
(define (print-board board)
  (for ([row (in-vector board)])
    (printf "~a~n" row)))

;; Handle player's guess on the opponent's board
(define (player-guess board mouseX mouseY)
  (let* ((board-pos (mouse-to-board mouseX mouseY x-offset opponent-y-offset)))
    (when board-pos
      (let ((row (car board-pos))
            (col (cdr board-pos)))
        (cond
          [(or (eq? (vector-ref (vector-ref board row) col) 'hit)
               (eq? (vector-ref (vector-ref board row) col) 'miss))
           (printf "Cell at ~a, ~a was already guessed. Try again!~n" row col)]
          [(eq? (vector-ref (vector-ref board row) col) #t)
           (vector-set! (vector-ref board row) col 'hit)
           (printf "Player hit at ~a, ~a!~n" row col)]
          [else
           (vector-set! (vector-ref (vector-ref board row) col) 'miss)
           (printf "Player missed at ~a, ~a.~n" row col)])))))

;; AI guess logic
(define (ai-guess board)
  (cond
    [(eq? ai-difficulty 'easy) (ai-easy-guess board)]
    [(eq? ai-difficulty 'medium) (ai-medium-guess board)]
    [(eq? ai-difficulty 'hard) (ai-hard-guess board)]))

;; Easy AI: Randomly guesses
(define (ai-easy-guess board)
  (let loop ()
    (let* ((row (random boardSize))
           (col (random boardSize)))
      (if (or (eq? (vector-ref (vector-ref board row) col) 'hit)
              (eq? (vector-ref (vector-ref board row) col) 'miss))
          (loop)
          (cond
            [(eq? (vector-ref (vector-ref board row) col) #t)
             (vector-set! (vector-ref board row) col 'hit)
             (printf "Easy AI hit at ~a, ~a!~n" row col)]
            [else
             (vector-set! (vector-ref board row) col 'miss)
             (printf "Easy AI missed at ~a, ~a.~n" row col)])))))

;; Medium AI: Targets adjacent cells after hitting
(define (ai-medium-guess board)
  (cond
    [(not (null? target-queue))
     (let* ((target (car target-queue))
            (row (car target))
            (col (cdr target)))
       (set! target-queue (cdr target-queue))
       (if (or (< row 0) (>= row boardSize) (< col 0) (>= col boardSize)
               (eq? (vector-ref (vector-ref board row) col) 'hit)
               (eq? (vector-ref (vector-ref board row) col) 'miss))
           (ai-medium-guess board)
           (cond
             [(eq? (vector-ref (vector-ref board row) col) #t)
              (vector-set! (vector-ref board row) col 'hit)
              (printf "Medium AI hit at ~a, ~a!~n" row col)
              (add-to-target-queue row (+ col 1))
              (add-to-target-queue row (- col 1))
              (add-to-target-queue (+ row 1) col)
              (add-to-target-queue (- row 1) col)]
             [else
              (vector-set! (vector-ref board row) col 'miss)
              (printf "Medium AI missed at ~a, ~a.~n" row col)])))]
    [else (ai-easy-guess board)]))

;; Helper function for adding targets
(define (add-to-target-queue row col)
  (when (and (>= row 0) (< row boardSize) (>= col 0) (< col boardSize))
    (set! target-queue (append target-queue (list (cons row col))))))

;; Hard AI: Always hits a ship
(define (ai-hard-guess board)
  (let loop ([r 0] [c 0])
    (cond
      [(>= r boardSize) (printf "Hard AI could not find any ships!~n")]
      [(>= c boardSize) (loop (+ r 1) 0)]
      [(eq? (vector-ref (vector-ref board r) c) #t)
       (vector-set! (vector-ref board r) c 'hit)
       (printf "Hard AI hit at ~a, ~a!~n" r c)]
      [else (loop r (+ c 1))])))

;; Game Over check
(define (check-game-over board)
  (let ([ship-part-count 0]
        [hit-part-count 0])
    (for ([row (in-vector board)])
      (for ([cell (in-vector row)])
        (when (eq? cell #t) (set! ship-part-count (+ ship-part-count 1)))
        (when (eq? cell 'hit) (set! hit-part-count (+ hit-part-count 1)))))
    (= ship-part-count hit-part-count)))

;; Update function
(define (update state)
  (let* ((mouseX (mouse-x))
         (mouseY (mouse-y))
         (mouseClicked (btn-mouse)))
    (cond
      [(and (eq? currentState home)
            (mouse-in? mouseX mouseY 340 225 button-width button-height)
            mouseClicked)
       (set! currentState game-mode-selection)]
      [(eq? currentState game-mode-selection)
       (when (and (mouse-in? mouseX mouseY 300 200 button-width button-height) mouseClicked)
         (set! game-mode '1-player-vs-ai)
         (set! currentState ai-difficulty-selection))
       (when (and (mouse-in? mouseX mouseY 300 300 button-width button-height) mouseClicked)
         (set! game-mode '2-player)
         (set! currentState ship-selection))]
      [(eq? currentState ai-difficulty-selection)
       (when (and (mouse-in? mouseX mouseY 300 200 button-width button-height) mouseClicked)
         (set! ai-difficulty 'easy)
         (set! currentState ship-selection))
       (when (and (mouse-in? mouseX mouseY 300 300 button-width button-height) mouseClicked)
         (set! ai-difficulty 'medium)
         (set! currentState ship-selection))
       (when (and (mouse-in? mouseX mouseY 300 400 button-width button-height) mouseClicked)
         (set! ai-difficulty 'hard)
         (set! currentState ship-selection))]
      ;; More update cases for other game states...
      )))

;; Draw function
(define (draw state)
  (begin
    (cls)
    (cond
      [(eq? currentState home)
       (color 7)
       (rect 300 200 button-width button-height #:fill #t)
       (color 0)
       (text 340 225 "Start Game")
       (font wide-font)
       (color 7)
       (text 300 100 "Welcome to Battleship!")]
      ;; More draw cases for other game states...
      )))

;; Restart Game logic
(define (restart-game)
  (set! player1-board (createBoard boardSize))
  (set! player2-board (createBoard boardSize))
  (set! opponentBoard (createBoard boardSize))
  (set! currentState home)
  (set! player1-ships-placed 0)
  (set! player2-ships-placed 0)
  (set! ships-placed-locations '())
  (set! playerTurn 0))

;; Game loop
(define (game-loop)
  (update currentState)
  (draw currentState))

(run game-loop 800 900 #:fps 60)
