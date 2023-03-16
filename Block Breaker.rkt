#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require racket/trace)

;; coordinates struct of x and y, they can be maximum 500
;; vx is velocity of x and vy is velocity of y
(struct state (x y vx vy) #:mutable #:transparent)
(define a-ball (state 250 300 2 -2))
(define bar (state 250 485 0 0))
(define ver-bar (state 5 255 0 0))
(define ver-bar1 (state 495 255 0 0))
(define hor-bar (state 250 5 0 0))

(define SCENE (empty-scene 500 500 "black"))
(define VERTICAL_RECTANGLE (rectangle 1 500 "solid" "red"))
(define HORIZONTAL_RECTANGLE (rectangle 490 1 "solid" "red"))
(define BALL (circle 6 "solid" "green"))
(define BAR (rectangle 50 5 "solid" "green"))
(define BLOCK (rectangle 30 8 "solid" "green"))

(define (MAKE_STRUCTURE start lines columns)
  
  (define (iterations HOW_MANY_TIMES lst)
    (cond
      [(= HOW_MANY_TIMES 1) (cons HOW_MANY_TIMES lst)]
      [#t (iterations (- HOW_MANY_TIMES 1) (cons HOW_MANY_TIMES lst))]))
  
  (define NUMBER-OF-ROWS (iterations lines `()))
  (define NUMBER-OF-COLUMNS (iterations columns `()))
  
  (define (MAKE-LINE start)  
    (for/list [(i NUMBER-OF-COLUMNS)]
      (cond
        [(= i 1) start]
        [#t (cons (+ (first start) (* (list-ref NUMBER-OF-COLUMNS (- i 2)) 35)) (rest start))]
        )
      )
    )
  (apply append (for/list [(i NUMBER-OF-ROWS)]
                  (cond
                    [(= i 1) (MAKE-LINE start)]
                    [#t (MAKE-LINE (cons (first start) (cons (+ (first (rest start)) (* (list-ref NUMBER-OF-ROWS (- i 2)) 13)) '())))]
                    )
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
    [(key=? a-key "left") (set-state-x! bar (- (state-x bar) 16))]
    [(key=? a-key "right") (set-state-x! bar (+ (state-x bar) 15))]
    [#t w] ; order-free checking
    )
  )
(define my-struct (list->vector (MAKE_STRUCTURE `(85 125) 10 10)))

(define (get-index1 elem a-vector i)
  (cond
    [(>= i (vector-length a-vector)) #f]
    [(eq? elem (vector-ref a-vector i)) i]
    [#t (get-index1 elem a-vector (+ i 1))]))
(define (get-index elem a-vector) (get-index1 elem a-vector 0))

(define (OBJECTS state)
  (define ball-pos (cons (state-x a-ball) (cons (state-y a-ball) '())))
  (define (hits-block? ball block)
    (cond
      [(empty? block) #f]
      [(and (<= (first ball) (+ (first block) 18))
            (>= (first ball) (- (first block) 18))
            (<= (second ball) (+ (second block) 7))
            (>= (second ball) (- (second block) 7))) #t]
      [#t #f]))
  (define (MAP_BALL struct)
    (for/vector ([i struct])
      (cond
        [(hits-block? ball-pos i) (vector-set! struct (get-index i struct) '())]
        [#t i])))
  (define objects (MY_FINAL_SCENE SCENE (vector->list (MAP_BALL my-struct))))
  (place-image BALL
               (state-x a-ball)
               (state-y a-ball)
               (place-image VERTICAL_RECTANGLE
                            (state-x ver-bar)
                            (state-y ver-bar)
                            (place-image VERTICAL_RECTANGLE
                                         (state-x ver-bar1)
                                         (state-y ver-bar1)
                                         (place-image HORIZONTAL_RECTANGLE
                                                      (state-x hor-bar)
                                                      (state-y hor-bar)
                                                      (place-image BAR
                                                                   (state-x bar)
                                                                   (state-y bar)
                                                                   objects))))))



(define (MY_FINAL_SCENE a-scene structure)
  (cond
    [(empty? structure) a-scene]
    [(void? (first structure)) (MY_FINAL_SCENE a-scene (rest structure))]
    [(empty? (first structure)) (MY_FINAL_SCENE a-scene (rest structure))]
    [#t (MY_FINAL_SCENE (place-image BLOCK (caar structure) (first (rest (first structure))) a-scene)
                        (rest structure))]
    ))

;(define (MAP_BALL structure)
 ; (define ball-pos (list (state-x a-ball) (state-y a-ball))))

(big-bang a-ball
  (on-tick UPDATE_POSITION 1/120)
  (to-draw OBJECTS)
  (on-key MOVE)
  )




  
