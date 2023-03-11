#lang racket/base
(require 2htdp/image)
(require 2htdp/universe)

;; coordinates struct of x and y, they can be maximum 500
;; vx is velocity of x and vy is velocity of y
(struct state (x y vx vy) #:mutable #:transparent)
(define a-ball (state 250 300 0 2))
(define bar (state 250 485 0 0))
(define a-block1 (state 250 150 0 0))
(define a-block2 (state 285 150 0 0))

(define SCENE (empty-scene 500 500 "black"))
(define VERTICAL_RECTANGLE (rectangle 1 500 "solid" "red"))
(define HORIZONTAL_RECTANGLE (rectangle 490 1 "solid" "red"))
(define BALL (circle 6 "solid" "green"))
(define BAR (rectangle 50 5 "solid" "green"))

;First Row Block1 = a-block1; also block1 is in the middle.
(define block1 (rectangle 30 7 "solid" "red"))
(define block2 (rectangle 30 7 "solid" "Brown"))
(define block3 (rectangle 30 7 "solid" "Green"))
(define block4 (rectangle 30 7 "solid" "Orange"))
(define block5 (rectangle 30 7 "solid" "Pink"))
(define block6 (rectangle 30 7 "solid" "Purple"))
(define block7 (rectangle 30 7 "solid" "Turquoise"))
(define block8 (rectangle 30 7 "solid" "Yellow"))
(define block9 (rectangle 30 7 "solid" "Goldenrod"))
(define block10(rectangle 30 7 "solid" "Dark Pink"))

;Second Row
(define block11 (rectangle 30 7 "solid" "Light Brown"))
(define block12 (rectangle 30 7 "solid" "Medium Brown"))
(define block13 (rectangle 30 7 "solid" "Dark Brown"))
(define block14 (rectangle 30 7 "solid" "Medium Cyan"))
(define block15 (rectangle 30 7 "solid" "Light Goldenrod"))
(define block16 (rectangle 30 7 "solid" "Medium Gray"))
(define block17 (rectangle 30 7 "solid" "Medium Green"))
(define block18 (rectangle 30 7 "solid" "Light Orange"))
(define block19 (rectangle 30 7 "solid" "Medium Orange"))
(define block20 (rectangle 30 7 "solid" "Medium Pink"))

;Third Row
(define block21 (rectangle 30 7 "solid" "red"))
(define block22 (rectangle 30 7 "solid" "Brown"))
(define block23 (rectangle 30 7 "solid" "Green"))
(define block24 (rectangle 30 7 "solid" "Orange"))
(define block25 (rectangle 30 7 "solid" "Pink"))
(define block26 (rectangle 30 7 "solid" "Purple"))
(define block27 (rectangle 30 7 "solid" "Turquoise"))
(define block28 (rectangle 30 7 "solid" "Yellow"))
(define block29 (rectangle 30 7 "solid" "Goldenrod"))
(define block30 (rectangle 30 7 "solid" "Dark Pink"))

(define (UPDATE_POSITION ball)
  (set-state-x! a-ball (+ (state-x a-ball) (state-vx a-ball)))
  (set-state-y! a-ball (+ (state-y a-ball) (state-vy a-ball)))
  (BOUNCE)
  )

(define (APPLY_VELOCITY)
  (cond
    [(and [equal? (state-vx a-ball) 0]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    [(and [equal? (state-vy a-ball) 0]) (set-state-vx! a-ball (* (state-vx a-ball) -1))]
    [(and [positive? (state-vx a-ball)]
          [positive? (state-vy a-ball)]
          [< (state-y a-ball) 476]) (set-state-vx! a-ball (* (state-vx a-ball) -1))]
    [(and [negative? (state-vx a-ball)]
          [negative? (state-vy a-ball)]
          [< (state-y a-ball) 476]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    [(and [negative? (state-vx a-ball)]
          [positive? (state-vy a-ball)]
          [< (state-y a-ball) 476]) (set-state-vx! a-ball (* (state-vx a-ball) -1))]
    [(and [positive? (state-vx a-ball)]
          [negative? (state-vy a-ball)]
          [< (state-y a-ball) 476]) (set-state-vx! a-ball (* (state-vx a-ball) -1))]

    ;; and now the two conditions for the bar
    [(and [positive? (state-vx a-ball)]
          [positive? (state-vy a-ball)]
          [>= (state-y a-ball) 476]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    [(and [negative? (state-vx a-ball)]
          [positive? (state-vy a-ball)]
          [>= (state-y a-ball) 476]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    )
  )

(define (BOUNCE)
  (cond
    [(> (state-x a-ball) 489) (APPLY_VELOCITY)]
    [(< (state-x a-ball) 11) (APPLY_VELOCITY)]
    [(< (state-y a-ball) 11) (APPLY_VELOCITY)]
    [(and [= (state-y a-ball) 476]
          [<= (- (state-x a-ball) (state-x bar)) 25]
          [>= (- (state-x a-ball) (state-x bar)) -25])
     (cond
       [(= (state-x a-ball) (state-x bar)) (APPLY_VELOCITY)]
       [(and [<= (- (state-x a-ball) (state-x bar)) 5]
             [>= (- (state-x a-ball) (state-x bar)) -5]) (APPLY_VELOCITY)]
       [(and [<= (- (state-x a-ball) (state-x bar)) 10]
             [>= (- (state-x a-ball) (state-x bar)) -10]) (APPLY_VELOCITY)]
       [(and [<= (- (state-x a-ball) (state-x bar)) 15]
             [>= (- (state-x a-ball) (state-x bar)) -15]) (APPLY_VELOCITY)]
       [(and [<= (- (state-x a-ball) (state-x bar)) 20]
             [>= (- (state-x a-ball) (state-x bar)) -20]) (APPLY_VELOCITY)]
       [(and [<= (- (state-x a-ball) (state-x bar)) 25]
             [>= (- (state-x a-ball) (state-x bar)) -25]) (APPLY_VELOCITY)]
       )]
  )
  )

(define (MOVE w a-key)
  (cond
    [(key=? a-key "left") (set-state-x! bar (- (state-x bar) 20))]
    [(key=? a-key "right") (set-state-x! bar (+ (state-x bar) 20))]
    [#t w] ; order-free checking
    )
  )

(define (OBJECTS state)
(place-image BALL
        (state-x a-ball)
        (state-y a-ball)
(place-image VERTICAL_RECTANGLE
        5
        255
(place-image VERTICAL_RECTANGLE
        495
        255
(place-image block1
       (state-x a-block1)
       (state-y a-block1)
(place-image block2
       (state-x a-block2)
       (state-y a-block2)
(place-image block3
       215
       150
(place-image block4
       180
       150
(place-image block5
       145
       150
(place-image block6
       110
       150
(place-image block7
       75
       150
(place-image block8
       320
       150
(place-image block9
       355
       150
(place-image block10
       390
       150
(place-image block11
       250
       137
(place-image block12
       285
       137
(place-image block13
       215
       137
(place-image block14
       180
       137
(place-image block15
       145
       137
(place-image block16
       110
       137
(place-image block17
       75
       137
(place-image block18
       320
       137
(place-image block19
       355
       137
(place-image block20
       390
       137
(place-image block21
       250
       125
(place-image block22
       285
       125
(place-image block23
       215
       125
(place-image block24
       180
       125
(place-image block25
       145
       125
(place-image block26
       110
       125
(place-image block27
       75
       125
(place-image block28
       320
       125
(place-image block29
       355
       125
(place-image block30
       390
       125
(place-image HORIZONTAL_RECTANGLE
       250
       5
(place-image BAR
(state-x bar)
(state-y bar)
SCENE))))))))))))))))))))))))))))))))))))

(big-bang a-ball
  (on-tick UPDATE_POSITION 1/120)
  (to-draw OBJECTS)
  (on-key MOVE)
  )




  
