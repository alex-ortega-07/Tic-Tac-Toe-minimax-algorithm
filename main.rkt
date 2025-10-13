;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname main) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)


(define SIZE 500) ;; can range from [300,900]
(define GAP (/ SIZE 10)) ;; border around the board

(define MTS (empty-scene SIZE SIZE "black"))

(define WIDTH SIZE) 
(define HEIGHT SIZE)
(define W (/ (- WIDTH (* 2 GAP)) 3)) ;; W(idth) of tic tac toe cell
(define H (/ (- HEIGHT (* 2 GAP)) 3)) ;; H(eight) of tic tac toe cell

(define PEN (make-pen "White"
                      (round (/ SIZE 40)) "solid" "round" "round"))

(define X 1)  ;; Val
(define O 0)  ;; Val
(define B 2) ; for Blank square

(define board-with-lines
  (add-line (add-line (add-line (add-line MTS GAP (+ GAP H) (+ GAP (* 3 W)) (+ GAP H)  PEN)
                                GAP (+ GAP (* 2 H)) (+ GAP (* 3 W)) (+ GAP (* 2 H)) PEN)
                      (+ GAP W) GAP (+ GAP W) (+ GAP (* 3 H)) PEN)
            (+ GAP (* 2 W)) GAP (+ GAP (* 2 W)) (+ GAP (* 3 H)) PEN))

(define FONT-SIZE  (floor (* SIZE 0.2)))
(define TEXT-SIZE  (floor (* SIZE 0.05)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct world-state (board turn state difficulty))
;; WorldState is a (make-world-state (listof Val[0,2]) Natural[0,2] Natural[0,3] Natural[0,9])
;; interp.  board contains the actual state of the board. X is 1.
;;                                                        O is 0.
;;                                                        B is 2 (blank).
;;          turn is the turn ofthe player. 0 means that it is the turn for Player 1.
;;                                         1 means that it is the turn for Player 2.
;;                                         2 means there is no turn.
;;          state is the state of the program. 0 means that the game is still running.
;;                                             1 means that Player 1 won.
;;                                             2 means that Player 2 won.
;;                                             3 means it is a draw.
;;          difficulty is the actual difficulty of the game being 0 the easiest and 9 the hardest

(define board
  (list B B B
        B B B
        B B B))

;; Default configuration of the world-state
(define START (make-world-state board 0 0 0))

;; main: WorldState -> WorldState
;; start with (main START)
(define (main ws)
  (big-bang ws
    (on-tick update)         ; WorldState -> WorldState
    (to-draw render-imgs)    ; WorldState -> Image
    (on-key key-handler)     ; WorldState KeyEvent -> WorldState
    (on-mouse mouse-event)))     ; WorldState Integer Integer MouseEvent -> WorldState


(define WINS
  (list
   (list 0 1 2)
   (list 3 4 5)
   (list 6 7 8)
   (list 0 3 6)
   (list 1 4 7)
   (list 2 5 8)
   (list 0 4 8)
   (list 2 4 6)))

;; wins?: Board Value -> Boolean
;; returns true if player won, otherwise returns false
(define (wins? bd player)
  (local [(define (read-unit lop)
            (map (lambda (pos)
                   (list-ref bd pos))
                 lop))

          (define (check-unit lop)
            (andmap (lambda(e) (= e player))
                    (read-unit lop)))]
    
    (ormap check-unit WINS)))

;; update: WorldState -> WorldState
;; updates the WorldState and checks the next movement of the computer if the board is not full
(define (update ws)
  (local [(define (get-blank-positions bd)
            (local [(define (get-blank-positions bd acc)
                      (cond [(empty? bd) empty]
                            [else
                             (if (= (first bd) B)
                                 (cons acc (get-blank-positions (rest bd) (add1 acc)))
                                 (get-blank-positions (rest bd) (add1 acc)))]))]

              (get-blank-positions bd 0)))

          (define (make-move level-difficulty)
            (local [(define list-blanks (get-blank-positions (world-state-board ws)))

                    (define (pick-best lob best)
                      (cond [(empty? lob) (first (rest best))]
                            [(> (first (first lob)) (first best))
                             (pick-best (rest lob) (first lob))]
                            [else
                             (pick-best (rest lob) best)]))

                    (define (best-move ws bd depth)
                      (local [(define (get-blanks b acc i)
                                (cond [(empty? b) (foldl cons empty acc)]
                                      [(= (first b) B)
                                       (get-blanks (rest b) (cons i acc) (add1 i))]
                                      [else
                                       (get-blanks (rest b) acc (add1 i))]))

                              (define blanks (get-blanks bd empty 0))

                              (define (score-move pos)
                                (local [(define new-bd (insert-number-board ws bd pos O))]
                                  
                                  (list (minimax new-bd X depth) new-bd)))

                              (define scored (map score-move blanks))]

                        (if (empty? scored)
                            bd
                            (pick-best (rest scored) (first scored)))))

                    (define (minimax bd player depth)
                      (local [(define (trivial? b d)
                                (or (wins? b X)
                                    (wins? b O)
                                    (board-full? b)
                                    (= d 0)))

                              (define (utility b)
                                (cond [(wins? b X) -1]
                                      [(wins? b O)  1]
                                      [(board-full? b) 0]
                                      [else 0]))

                              (define (get-blank-positions b acc)
                                (cond [(empty? b) empty]

                                      [(= (first b) B)
                                       (cons acc (get-blank-positions (rest b) (add1 acc)))]
                                      
                                      [else
                                       (get-blank-positions (rest b) (add1 acc))]))

                              (define (next-boards b p)
                                (map (λ (pos) (insert-number-board ws b pos p))
                                     (get-blank-positions b 0)))]

                        (cond [(trivial? bd depth) (utility bd)]

                              [(= player O)
                               (apply max
                                      (map (λ (b) (minimax b X (sub1 depth)))
                                           (next-boards bd O)))]

                              [(= player X)
                               (apply min
                                      (map (λ (b) (minimax b O (sub1 depth)))
                                           (next-boards bd X)))])))
                    
                    (define (new-board d)
                      (cond [(= d 0)
                             (insert-number-board ws
                                                  (world-state-board ws)
                                                  (list-ref list-blanks (random (length list-blanks))) O)]
                            [(= d 1)
                             (if (not (false? (check-win list-blanks)))
                                 (check-win list-blanks)
                                 (new-board 0))]

                            [(= d 2)
                             (if (not (false? (check-win list-blanks)))
                                 (check-win list-blanks)
                                 (if (not (false? (check-block list-blanks)))
                                     (check-block list-blanks)
                                     (new-board 0)))]

                            [(and (>= d 3) (<= d 9))
                             (best-move ws (world-state-board ws) d)]))

                    (define (check-win lob)
                      (cond [(empty? lob) false]
                            [else
                             (local [(define try (insert-number-board ws (world-state-board ws) (first lob) O))]
                                                
                               (if (wins? try O)
                                   try
                                   (check-win (rest lob))))]))
                    
                    (define (check-block lob)
                      (cond [(empty? lob) false]
                            [else
                             (local [(define (try n) (insert-number-board ws (world-state-board ws) (first lob) n))]

                               (if (wins? (try X) X)
                                   (try O)
                                   (check-block (rest lob))))]))]
              
              (local [(define new-board-return (new-board level-difficulty))]
                      
                (make-world-state
                 new-board-return
                 (if (wins? new-board-return O)
                     2
                     (change-turn (world-state-turn ws)))
                 (if (wins? new-board-return O)
                     2
                     (world-state-state ws))
                 (world-state-difficulty ws)))))
          

          (define (board-full? board)
            (cond [(empty? (get-blank-positions board)) true]
                  [else false]))]

    (cond 
      [(board-full? (world-state-board ws))
       (make-world-state
        (world-state-board ws)
        2
        (if (wins? (world-state-board ws) X)
            1
            3)
        (world-state-difficulty ws))]
          
      [(= (world-state-turn ws) 1)
       (make-move (world-state-difficulty ws))]
          
      [else ws])))


;; insert-number-board: WorldState Board Natural -> Board
;; Inserts the correct player depending on the turn into the board at index
(define (insert-number-board ws bd index player)
  (local [(define (insert-number-board bd acc)
            (cond [(empty? bd) empty]
                  [else
                   (cons (if (= acc index)
                             player
                             (first bd))
                         (insert-number-board (rest bd) (add1 acc)))]))]

    (insert-number-board bd 0)))


;; change-turn: Natural -> Natural
;; Changes the turn to be the next turn
(define (change-turn t) (modulo (add1 t) 2))

;; render-imgs: WorldState -> Image
;; Renders the world state to the screen including all the board and text
(define (render-imgs ws)
  (local [(define (difficulty-text n)
            (cond [(= n 0) "0 (Very easy)"]
                  [(= n 1) "1 (Easy)"]
                  [(= n 2) "2 (Medium)"]
                  [(= n 3) "3 (Hard)"]
                  [(= n 4) "4 (Hard)"]
                  [(= n 5) "5 (Hard)"]
                  [(= n 6) "6 (Hard)"]
                  [(= n 7) "7 (Hard)"]
                  [(= n 8) "8 (Hard)"]
                  [(= n 9) "9 (Hard)"]))

          (define (render-board-text bd acc bg)
            (cond [(empty? bd) bg]
                  [else
                   (place-image
                    (cond [(= (first bd) O) (text "O" FONT-SIZE "Green")]
                          [(= (first bd) X) (text "X" FONT-SIZE "Purple")]
                          [(= (first bd) B) (text " " FONT-SIZE "Black")]
                          [else "We won't get to this point"])
                    (+ (/ W 2) (* W (modulo acc 3)) GAP)
                    (+ (/ H 2) (* H (floor (/ acc 3))) GAP)
                    (render-board-text (rest bd) (add1 acc) bg))]))
          
          (define (winner-display bg)
            (local [(define (txt t) (text t (floor (/ FONT-SIZE 2)) "RED"))]
              
              (cond [(= (world-state-state ws) 1) (place-image (txt "X WINS") (/ WIDTH 2) (/ HEIGHT 2)
                                                               (place-image (rectangle (image-width (txt "X WINS")) (image-height (txt "X WINS")) "solid" "grey") (/ WIDTH 2) (/ HEIGHT 2) bg))]
                      
                    [(= (world-state-state ws) 2) (place-image (txt "O WINS") (/ WIDTH 2) (/ HEIGHT 2)
                                                               (place-image (rectangle (image-width (txt "O WINS")) (image-height (txt "O WINS")) "solid" "grey") (/ WIDTH 2) (/ HEIGHT 2) bg))]

                    [(= (world-state-state ws) 3) (place-image (txt "DRAW") (/ WIDTH 2) (/ HEIGHT 2)
                                                               (place-image (rectangle (image-width (txt "DRAW")) (image-height (txt "DRAW")) "solid" "grey") (/ WIDTH 2) (/ HEIGHT 2) bg))]
                    [else bg])))]
    
    (winner-display (render-board-text (world-state-board ws) 0
                                       (place-image (text (string-append "Difficulty: " (difficulty-text (world-state-difficulty ws))) TEXT-SIZE "Yellow") (+ (/ WIDTH 2)) (/ GAP 2)
                                                    (place-image board-with-lines (/ WIDTH 2) (/ HEIGHT 2)
                                                                 MTS))))))



;; mouse-event: WorldState Integer Integer MouseEvent -> WorldState
;; Checks for a correct click and we insert X into board at the box clicked
(define (mouse-event ws x y me)
  (local [(define (get-board-number bd)
            (local [(define (center-x n)
                      (+ (/ W 2) (* W (modulo n 3)) GAP))
                    
                    (define (center-y n)
                      (+ (/ H 2) (* H (floor (/ n 3))) GAP))
                     
                    (define (get-board-number bd acc)
                      (cond [(>= acc (length bd)) -1]
                            [else
                             (cond [(and (and (>= x (+ (- (center-x acc) (/ W 2)) (/ (pen-width PEN) 2)))
                                              (<= x (- (+ (center-x acc) (/ W 2)) (/ (pen-width PEN) 2))))
                                         (and (>= y (+ (- (center-y acc) (/ H 2)) (/ (pen-width PEN) 2)))
                                              (<= y (- (+ (center-y acc) (/ H 2)) (/ (pen-width PEN) 2)))))
                                    acc]
                         
                                   [else
                                    (get-board-number bd (add1 acc))])]))]
              
              (get-board-number bd 0)))

          (define bd-number (get-board-number (world-state-board ws)))
          (define correct-click? (and (not (= bd-number -1)) (= (list-ref (world-state-board ws) bd-number) B)))

          (define change-board
            (insert-number-board ws (world-state-board ws) bd-number X))]

    (cond [(and (mouse=? me "button-down") correct-click? (= 0 (world-state-turn ws)))
           (make-world-state
            change-board
            (if (wins? change-board X)
                2
                (change-turn (world-state-turn ws)))
            (if (wins? change-board X)
                1
                (world-state-state ws))
            (world-state-difficulty ws))]
          [else ws])))


;; key-handler: WorldState KeyEvent -> WorldState
;; Changes the difficulty of the game when a number in the keyboard is pressed
(define (key-handler ws ke)
  (make-world-state
   (world-state-board ws)
   (world-state-turn ws)
   (world-state-state ws)
   (if (not (false? (string->number ke)))
       (string->number ke)
       (world-state-difficulty ws))))
