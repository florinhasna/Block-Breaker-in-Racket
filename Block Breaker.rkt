#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require racket/trace)

;; coordinates struct of x and y, they can be maximum 500
;; vx is velocity of x and vy is velocity of y
(struct state (x y vx vy) #:mutable #:transparent)
(define a-ball (state 250 300 -1 2))
(define bottom-bar (state 250 485 35 0))
(define left-wall (state 5 255 0 0))
(define right-wall (state 495 255 0 0))
(define ceiling (state 250 5 0 0))

(define SCENE (empty-scene 500 500 "black"))
(define VERTICAL_RECTANGLE (rectangle 1 500 "solid" "red"))
(define HORIZONTAL_RECTANGLE (rectangle 490 1 "solid" "red"))
(define BALL (circle 6 "solid" "green"))
(define BAR (rectangle 50 5 "solid" "green"))
(define BLOCK (rectangle 30 8 "solid" "green"))

; creates a list of pairs, each pair has 2 elements representing the position-x and position-y of a block on the scene
(define (MAKE_STRUCTURE start lines columns)
  
  (define NUMBER-OF-ROWS (rest (range (+ lines 1))))      ; defined two lists to iterate the number of line times and column times
  (define NUMBER-OF-COLUMNS (rest (range (+ columns 1))))

  ; creates a list of pairs representing a line of blocks on the scene
  (define (MAKE-LINE start)  
    (for/list [(i NUMBER-OF-COLUMNS)]
      (cond
        [(= i 1) start]
        [#t (cons (+ (first start) (* (list-ref NUMBER-OF-COLUMNS (- i 2)) 35)) (rest start))]
        )
      )
    )

  ; creates the number the rows using MAKE-LINE
  (apply append (for/list [(i NUMBER-OF-ROWS)]
                  (cond
                    [(= i 1) (MAKE-LINE start)]
                    [#t (MAKE-LINE (cons (first start) (cons (+ (first (rest start)) (* (list-ref NUMBER-OF-ROWS (- i 2)) 13)) '())))]
                    )
                  )
         )
  
  ) ; end of MAKE_STRUCTURE

; making the ball move
(define (UPDATE_POSITION ball)
  (set-state-x! a-ball (+ (state-x a-ball) (state-vx a-ball)))
  (set-state-y! a-ball (+ (state-y a-ball) (state-vy a-ball)))
  (BOUNCE)
  )

(define (APPLY_VELOCITY)
  (cond
    [(and [positive? (state-vx a-ball)]
          [positive? (state-vy a-ball)]
          [< (state-y a-ball) (+ (state-y bottom-bar) 8.5)]
          [> (state-y a-ball) (- (state-x bottom-bar) 8.5)]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    [(and [negative? (state-vx a-ball)]
          [positive? (state-vy a-ball)]
          [< (state-y a-ball) (+ (state-y bottom-bar) 8.5)]
          [> (state-x a-ball) (- (state-x bottom-bar) 8.5)]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    
    [(and [equal? (state-vx a-ball) 0]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    [(and [equal? (state-vy a-ball) 0]) (set-state-vx! a-ball (* (state-vx a-ball) -1))]
    [(and [positive? (state-vx a-ball)]
          [positive? (state-vy a-ball)]) (set-state-vx! a-ball (* (state-vx a-ball) -1))]
    [(and [negative? (state-vx a-ball)]
          [negative? (state-vy a-ball)]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    [(and [negative? (state-vx a-ball)]
          [positive? (state-vy a-ball)]) (set-state-vx! a-ball (* (state-vx a-ball) -1))]
    [(and [positive? (state-vx a-ball)]
          [negative? (state-vy a-ball)]) (set-state-vx! a-ball (* (state-vx a-ball) -1))]
    )
  )

(define (BOUNCE)
  (define ball-pos (cons (state-x a-ball) (cons (state-y a-ball) '())))
  (cond
    [(and (< (second ball-pos) (+ (state-y left-wall) 250))
          (> (second ball-pos) (- (state-y left-wall) 250))
          (< (first ball-pos) (+ (state-x left-wall) 6.5))) (APPLY_VELOCITY)]
    [(and (< (second ball-pos) (+ (state-y right-wall) 250))
          (> (second ball-pos) (- (state-y right-wall) 250))
          (> (first ball-pos) (- (state-x right-wall) 6.5))) (APPLY_VELOCITY)]
    [(and (< (first ball-pos) (+ (state-x ceiling) 250))
          (> (first ball-pos) (- (state-x ceiling) 250))
          (< (second ball-pos) (+ (state-y ceiling) 6.5))) (APPLY_VELOCITY)]
    [(and (< (second ball-pos) (+ (state-y bottom-bar) 8.5))
          (> (second ball-pos) (- (state-y bottom-bar) 8.5))
          (< (first ball-pos) (+ (state-x bottom-bar) 31))
          (> (first ball-pos) (- (state-x bottom-bar) 31))) (APPLY_VELOCITY)]))

; make the bar move using left and right arrows
(define (MOVE w a-key)
  (cond
    [(key=? a-key "left") (set-state-x! bottom-bar (- (state-x bottom-bar) (state-vx bottom-bar)))]
    [(key=? a-key "right") (set-state-x! bottom-bar (+ (state-x bottom-bar) (state-vx bottom-bar)))]
    [#t w] ; order-free checking
    )
  )

(define my-struct (list->vector (MAKE_STRUCTURE `(85 125) 5 10)))

(define (get-index1 elem a-vector i)
  (cond
    [(>= i (vector-length a-vector)) #f]
    [(eq? elem (vector-ref a-vector i)) i]
    [#t (get-index1 elem a-vector (+ i 1))]))
(define (get-index elem a-vector) (get-index1 elem a-vector 0))

(define (OBJECTS state)
  (define ball-position (cons (state-x a-ball) (cons (state-y a-ball) '())))
  (define (hits-block? ball block)
    (cond
      [(empty? block) #f]
      [(and (<= (first ball) (+ (first block) 18))
            (>= (first ball) (- (first block) 18))
            (<= (second ball) (+ (second block) 10))
            (>= (second ball) (- (second block) 10))) #t]
      [#t #f]))
    (define (MAP_BALL struct)
      (for/vector ([i struct])
        (cond
          [(hits-block? ball-position i) (vector-set! struct (get-index i struct) '())
                                         (APPLY_VELOCITY)]
          [#t i])))
    (define objects (MY_FINAL_SCENE SCENE (vector->list (MAP_BALL my-struct))))
    (place-image BALL
                 (state-x a-ball)
                 (state-y a-ball)
                 (place-image VERTICAL_RECTANGLE
                            (state-x left-wall)
                            (state-y left-wall)
                            (place-image VERTICAL_RECTANGLE
                                         (state-x right-wall)
                                         (state-y right-wall)
                                         (place-image HORIZONTAL_RECTANGLE
                                                      (state-x ceiling)
                                                      (state-y ceiling)
                                                      (place-image BAR
                                                                   (state-x bottom-bar)
                                                                   (state-y bottom-bar)
                                                                   objects))))))



(define (MY_FINAL_SCENE a-scene structure)
  (cond
    [(empty? structure) a-scene]
    [(void? (first structure)) (MY_FINAL_SCENE a-scene (rest structure))]
    [(empty? (first structure)) (MY_FINAL_SCENE a-scene (rest structure))]
    [#t (MY_FINAL_SCENE (place-image BLOCK (caar structure) (first (rest (first structure))) a-scene)
                        (rest structure))]
    ))

(big-bang a-ball
  (on-tick UPDATE_POSITION 1/120)
  (to-draw OBJECTS)
  (on-key MOVE)
  )
