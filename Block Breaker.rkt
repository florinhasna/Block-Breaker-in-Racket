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
(define BLOCK (rectangle 30 7 "solid" "green"))

     ; start must be a list of 2 elements where the first is the position on x and second position on y
(define (MAKE-STRUCTURE start lines columns) ; lines give the number of rows we ask for and similar with columns
  
  (define (iterations HOW_MANY_TIMES lst) ; creates an ordered list to help us iterate a certain number of times 
    (cond                                 ; based on lines and columns
      [(= HOW_MANY_TIMES 1) (cons HOW_MANY_TIMES lst)]
      [#t (iterations (- HOW_MANY_TIMES 1) (cons HOW_MANY_TIMES lst))]))
  
  (define NUMBER-OF-ROWS (iterations lines `()))        ; creates the ordered list for rows
  (define NUMBER-OF-COLUMNS (iterations columns `()))   ; creates the ordered list for columns
  
  (define (MAKE-LINE start)                 ; creates a full row at a distance of 35 on x
    (for/list [(i NUMBER-OF-COLUMNS)]
      (cond
        [(= i 1) start]
        [#t (cons (+ (first start) (* (list-ref NUMBER-OF-COLUMNS (- i 2)) 35)) (rest start))]
        )
      )
    )
  (for/list [(i NUMBER-OF-ROWS)]            ; uses MAKE-LINE to create multiple rows at a distance of 13 on y
    (cond
      [(= i 1) (MAKE-LINE start)]
      [#t (MAKE-LINE (cons (first start) (cons (+ (first (rest start)) (* (list-ref NUMBER-OF-ROWS (- i 2)) 13)) '())))]
      )
    )
  )

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




  
