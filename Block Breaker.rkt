#lang racket/base
(require 2htdp/image)
(require 2htdp/universe)

;; coordinates struct of x and y, they can be maximum 500
;; vx is velocity of x and vy is velocity of y
(struct state (x y vx vy) #:mutable #:transparent)
(define a-ball (state 250 300 0 2))
(define bar (state 250 485 0 0))

(define SCENE (empty-scene 500 500 "black"))
(define VERTICAL_RECTANGLE (rectangle 1 500 "solid" "red"))
(define HORIZONTAL_RECTANGLE (rectangle 490 1 "solid" "red"))
(define BALL (circle 6 "solid" "green"))
(define BAR (rectangle 50 5 "solid" "green"))

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
    [(key=? a-key "left") (set-state-x! bar (- (state-x bar) 5))]
    [(key=? a-key "right") (set-state-x! bar (+ (state-x bar) 5))]
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
                                         (place-image HORIZONTAL_RECTANGLE
                                                      250
                                                      5
                                                      (place-image BAR
                                                      (state-x bar)
                                                      (state-y bar)
                                                      SCENE))))))

(big-bang a-ball
  (on-tick UPDATE_POSITION 1/120)
  (to-draw OBJECTS)
  (on-key MOVE)
  )




  