#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require json)

;(define input-config (open-input-file "config.json"))
;(define config (string->jsexpr (read-line input-config)))
;(define output-config (open-output-file "config.json" #:exists 'replace))

(define theme-instruction (make-vector 1))
;; to save the index of the difficulty instruction

;(define music-instruction (make-vector 1))
;(vector-set! theme-instruction 0 0)
;(vector-set! music-instruction 0 (hash-ref config 'Music))

(define difficulty-instruction (make-vector 1))
(vector-set! difficulty-instruction 0 0)
;; to save the index of the difficulty instruction
;; our range of colours
(define color-pallete '("green" "lightpurple" "lightblue" "orange" "pink" "yellow"))

;(define file-to-write
;  (string-append "{"
;                 " \"Difficulty\":" (vector-ref difficulty-instruction 0)
;                 " \"Theme\":" (vector-ref theme-instruction 0)
;                 " \"Music\":" (vector-ref music-instruction 0)
;                 "}"))

;; coordinates struct of x and y, they can be maximum 500
;; vx is velocity of x and vy is velocity of y
(struct state (x y vx vy) #:mutable #:transparent)
(define a-ball (state 250 300 0 0))
(define bottom-bar (state 250 485 35 0))
(define left-wall (state 5 255 0 0))
(define right-wall (state 495 255 0 0))
(define ceiling (state 250 5 0 0))

; lets now define our objects
(define SCENE (empty-scene 500 500 "black"))
(define VERTICAL_RECTANGLE (rectangle 1 500 "solid" "red"))
(define HORIZONTAL_RECTANGLE (rectangle 490 1 "solid" "red"))

;; ----------------------------------------------------------------------------------------------------------------------------
;; ------------------------------------------------- HELPER FUNCTIONS SECTION !!! ---------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------

; creates a list of pairs, each pair has 2 elements representing the position-x and position-y of a block on the scene
(define (MAKE_STRUCTURE start lines columns)
  
  ; defined two lists to iterate the number of line times and column times
  (define NUMBER-OF-ROWS (rest (range (+ lines 1))))      
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

  ; creates the number of rows using MAKE-LINE
  (apply append (for/list [(i NUMBER-OF-ROWS)]
                  (cond
                    [(= i 1) (MAKE-LINE start)]
                    [#t (MAKE-LINE (cons (first start) (cons (+ (first (rest start)) (* (list-ref NUMBER-OF-ROWS (- i 2)) 13)) '())))]
                    )
                  )
         )
  ) ; end of MAKE_STRUCTURE

; created this function to check wether or not the ball is colliding with a block
(define (hits-block? ball block)
  (cond
    [(empty? block) #f]
    [(and (<= (first ball) (+ (first block) 18))
          (>= (first ball) (- (first block) 18))
          (<= (second ball) (+ (second block) 10))
          (>= (second ball) (- (second block) 10))) #t]
      [#t #f]))

(define (find-corners a-block)
  (cond
    [(not (empty? a-block)) (define left-margin (- (first a-block) 15)) ; left margin of the block
                            (define right-margin (+ (first a-block) 15)) ; right margin of the block
                            (define upper-margin (- (second a-block) 4)) ; upper margin of the block
                            (define lower-margin (+ (second a-block) 4)) ; lower margin of the block
                              (append `((,(- (+ left-margin 2) 6) ,(- (+ upper-margin 2) 6)))
                                      `((,(- (+ left-margin 2) 6) ,(+ (- lower-margin 2) 6)))
                                      `((,(+ (- right-margin 2) 6) ,(- (+ upper-margin 2) 6)))
                                      `((,(+ (- right-margin 2) 6) ,(+ (- lower-margin 2) 6))))]
    [#t #f])
  )

  (define (hits-corner? ball corners)
    (cond
      [(empty? corners) #f]
      [(and [>= (first ball) (caar corners)]
            [<= (first ball) (+ (caar corners) 2)]
            [>= (second ball) (second (first corners))]
            [<= (second ball) (+ (second (first corners)) 2)])
       "top-left"]
      [(and [>= (first ball) (caar corners)]
          [<= (first ball) (+ (caar corners) 2)]
          [<= (second ball) (second (first corners))]
          [>= (second ball) (- (second (first corners)) 2)])
       "bottom-left"]
      [(and [<= (first ball) (caar corners)]
            [>= (first ball) (- (caar corners) 2)]
            [>= (second ball) (second (first corners))]
            [<= (second ball) (+ (second (first corners)) 2)])
       "top-right"]
      [(and [>= (first ball) (caar corners)]
            [<= (first ball) (- (caar corners) 2)]
            [>= (second ball) (second (first corners))]
            [<= (second ball) (- (second (first corners)) 2)])
       "bottom-right"]
      [#t (hits-corner? ball (rest corners))]
      ))

; this function checks if the element is member of the vector and returns its index number
(define (get-index1 elem a-vector i)
  (cond
    [(>= i (vector-length a-vector)) #f]
    [(eq? elem (vector-ref a-vector i)) i]
    [#t (get-index1 elem a-vector (+ i 1))]))
; simplify the above by giving i=0
(define (get-index elem a-vector) (get-index1 elem a-vector 0))

; using the function created above, creates a list of pairs which are positions of the blocks
; then converts it to a vector because they are mutable and we can remove the blocks from the scene
(define list-struct (MAKE_STRUCTURE `(85 125) 5 10))
(define my-struct (list->vector list-struct))

; this function returns the placed blocks on the scene using the positions created in MAKE_STRUCTURE
; it is a recursion that when the first element is void? or empty? it removes that element and applies it on the others
(define (MY_FINAL_SCENE a-scene structure object)
  (cond
    [(empty? structure) a-scene]
    [(void? (first structure)) (MY_FINAL_SCENE a-scene (rest structure) object)]
    [(empty? (first structure)) (MY_FINAL_SCENE a-scene (rest structure) object)]
    [#t (MY_FINAL_SCENE (place-image object (caar structure) (first (rest (first structure))) a-scene)
                        (rest structure) object)]
    )
  )

;; ----------------------------------------------------------------------------------------------------------------------------
;; ---------------------------------------------- BALL AND BAR MOVEMENT SECTION !!! -------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------

; making the ball move by adding velocity to the position with every tick
; bounce the ball if it is the case
(define (UPDATE_POSITION ball)
  (cond
    [(eq? (vector-ref STATE-VECTOR 0) "start")
     (set-state-x! a-ball (+ (state-x a-ball) (state-vx a-ball)))
     (set-state-y! a-ball (+ (state-y a-ball) (state-vy a-ball)))
     (BOUNCE)])
  )

; a function that makes the ball bounce when hits the left wall
(define (APPLY_VELOCITY_FROM_LEFT)
  (cond
    [(and [equal? (state-vx a-ball) 0]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    [(and [negative? (state-vx a-ball)]
          [negative? (state-vy a-ball)]) (set-state-vx! a-ball (* (state-vx a-ball) -1))]
    [(and [negative? (state-vx a-ball)]
          [positive? (state-vy a-ball)]) (set-state-vx! a-ball (* (state-vx a-ball) -1))]
    )
  )

; a function that makes the ball bounce when hits the right wall
(define (APPLY_VELOCITY_FROM_RIGHT)
  (cond
    [(and [equal? (state-vx a-ball) 0]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    [(and [positive? (state-vx a-ball)]
          [positive? (state-vy a-ball)]) (set-state-vx! a-ball (* (state-vx a-ball) -1))]
    [(and [positive? (state-vx a-ball)]
          [negative? (state-vy a-ball)]) (set-state-vx! a-ball (* (state-vx a-ball) -1))]
    )
  )

; a function that makes the ball bounce when hits the top bar
(define (APPLY_VELOCITY_FROM_CEILING)
  (cond
    [(and [equal? (state-vy a-ball) 0]) (set-state-vx! a-ball (* (state-vx a-ball) -1))]
    [(and [negative? (state-vx a-ball)]
          [negative? (state-vy a-ball)]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    [(and [positive? (state-vx a-ball)]
          [negative? (state-vy a-ball)]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    )
  )

; a function that makes the ball bounce when hits the bottom bar
(define (APPLY_VELOCITY_FROM_BAR)
  (define randomiser (string->number (real->decimal-string (/ (random 10 30) 10))))
  (cond
    [(and [equal? (state-vx a-ball) 0]) (set-state-vy! a-ball (* (state-vy a-ball) -1))
                                        (set-state-vx! a-ball (+ (* (state-vx a-ball) -1) randomiser))]
    [(and [positive? (state-vx a-ball)]
          [positive? (state-vy a-ball)]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    [(and [negative? (state-vx a-ball)]
          [positive? (state-vy a-ball)]) (set-state-vy! a-ball (* (state-vy a-ball) -1))]
    )
  )

; this function checks the position of the ball when it needs to bounce
; and bounces it accordingly
(define (BOUNCE)
  (define ball-pos (cons (state-x a-ball) (cons (state-y a-ball) '())))
  (cond
    ; condition for when the ball hits the left wall
    [(and (<= (second ball-pos) (+ (state-y left-wall) 250))
          (>= (second ball-pos) (- (state-y left-wall) 250))
          (<= (first ball-pos) (+ (state-x left-wall) 6.5))) (APPLY_VELOCITY_FROM_LEFT)]

    ;  condition for when the ball hits the right wall
    [(and (<= (second ball-pos) (+ (state-y right-wall) 250))
          (>= (second ball-pos) (- (state-y right-wall) 250))
          (>= (first ball-pos) (- (state-x right-wall) 6.5))) (APPLY_VELOCITY_FROM_RIGHT)]

    ; condition for when the ball hits the top bar
    [(and (<= (first ball-pos) (+ (state-x ceiling) 250))
          (>= (first ball-pos) (- (state-x ceiling) 250))
          (<= (second ball-pos) (+ (state-y ceiling) 6.5))) (APPLY_VELOCITY_FROM_CEILING)]

    ; condition for when the ball hits the bottom bar
    [(and (<= (second ball-pos) (+ (state-y bottom-bar) 8.5))
          (>= (second ball-pos) (- (state-y bottom-bar) 8.5))
          (<= (first ball-pos) (+ (state-x bottom-bar) 31))
          (>= (first ball-pos) (- (state-x bottom-bar) 31))) (APPLY_VELOCITY_FROM_BAR)]
    ))

; make the bar move using left and right arrows
(define (MOVE w a-key)
  (cond
    [(eq? (vector-ref STATE-VECTOR 0) "start") (cond
                                           [(key=? a-key "left")
                                            (if [<= (state-x bottom-bar) 55]
                                                (set-state-x! bottom-bar 30)
                                                (set-state-x! bottom-bar (- (state-x bottom-bar) (state-vx bottom-bar))))]
                                           [(key=? a-key "right")
                                            (if [>= (state-x bottom-bar) 445]
                                                (set-state-x! bottom-bar 470)
                                                (set-state-x! bottom-bar (+ (state-x bottom-bar) (state-vx bottom-bar))))]
                                           )]
                                         ));; 470 on right ;; 30 on left

;; ----------------------------------------------------------------------------------------------------------------------------
;; ------------------------------------------------ SCENE AND OBJECT DRAWING !!! ----------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------

;; below there is created the main-menu scene
(define (MAIN_MENU)
  (define block-text (text/font "BLOCK" 40 (list-ref color-pallete (vector-ref theme-instruction 0)) #f 'decorative 'slant 'bold #f))
  (define breaker-text (text/font "BREAKER" 50 (list-ref color-pallete (vector-ref theme-instruction 0)) #f 'decorative 'slant 'bold #f))
  (define start (text "Start" 30 (list-ref color-pallete (vector-ref theme-instruction 0))))
  (define settings (text "Settings" 30 (list-ref color-pallete (vector-ref theme-instruction 0))))
  (define exit (text "Exit" 30 (list-ref color-pallete (vector-ref theme-instruction 0))))
  (define surroundings (rectangle 150 50 "outline" (list-ref color-pallete (vector-ref theme-instruction 0))))
  
  (place-image block-text
               180
               80
               (place-image breaker-text
                            280
                            130
                            (place-image surroundings
                                         250
                                         240
                                         (place-image surroundings
                                                      250
                                                      300
                                                      (place-image surroundings
                                                                   250
                                                                   360
                                                                   (place-image start
                                                                                250
                                                                                240
                                                                                (place-image settings
                                                                                             250
                                                                                             300
                                                                                             (place-image exit
                                                                                                          250
                                                                                                          360
                                                                                                          SCENE))))))))
  )

;; there is the playground, where the player has to break the blocks
(define (PLAYGROUND)
  (define BALL (circle 6 "solid" (list-ref color-pallete (vector-ref theme-instruction 0))))
  (define BAR (rectangle 50 5 "solid" (list-ref color-pallete (vector-ref theme-instruction 0))))
  (define BLOCK (rectangle 30 8 "solid" (list-ref color-pallete (vector-ref theme-instruction 0))))

  ;; set the velocity of the ball based on difficulty
  (if (not (negative? (state-vy a-ball)))
      (set-state-vy! a-ball (+ (vector-ref difficulty-instruction 0) 1))
      #t)
  
  (define ball-position (cons (state-x a-ball) (cons (state-y a-ball) '())))
  
    ; this function uses hits_block? to check if it has collided, then what side of the block the ball touched
    ; bounces the ball accordingly and removes the block from the scene by setting the vector element to an empty list
    (define (MAP_BALL struct)
      (for/vector ([i struct])
        (define my-corners (find-corners i))
        (cond
          [(hits-block? ball-position i)
           (cond
             [(eq? (hits-corner? ball-position my-corners) "top-left") (set-state-vx! a-ball (* (state-vx a-ball) -1))
                                                                       (set-state-vy! a-ball (* (state-vy a-ball) -1))]
             [(eq? (hits-corner? ball-position my-corners) "bottom-left") (set-state-vx! a-ball (* (state-vx a-ball) -1))
                                                                          (set-state-vy! a-ball (* (state-vy a-ball) -1))]
             [(eq? (hits-corner? ball-position my-corners) "top-right") (set-state-vx! a-ball (* (state-vx a-ball) -1))
                                                                        (set-state-vy! a-ball (* (state-vy a-ball) -1))]
             [(eq? (hits-corner? ball-position my-corners) "bottom-right") (set-state-vx! a-ball (* (state-vx a-ball) -1))
                                                                           (set-state-vy! a-ball (* (state-vy a-ball) -1))]
             
             ; condition for when hits the left side of the block
             [(and (>= (first ball-position) (- (first i) 18))
                   (<= (first ball-position) (- (first i) 10))
                   (<= (second ball-position) (+ (second i) 10))
                   (>= (second ball-position) (- (second i) 10)))
              (APPLY_VELOCITY_FROM_RIGHT)]
             
             ; condition for when it hits the right side of the block
             [(and (>= (first ball-position) (+ (first i) 10))
                   (<= (first ball-position) (+ (first i) 18))
                   (<= (second ball-position) (+ (second i) 10))
                   (>= (second ball-position) (- (second i) 10)))
              (APPLY_VELOCITY_FROM_LEFT)]

             ; condition for when it hits the lower side of the block
             [(and (>= (first ball-position) (- (first i) 18))
                   (<= (first ball-position) (+ (first i) 18))
                   (<= (second ball-position) (+ (second i) 10))
                   (>= (second ball-position) (+ (second i) 5)))
              (APPLY_VELOCITY_FROM_CEILING)]

             ; condition for when it hits the upper side of the block
             [(and (>= (first ball-position) (- (first i) 18))
                   (<= (first ball-position) (+ (first i) 18))
                   (>= (second ball-position) (- (second i) 10))
                   (<= (second ball-position) (- (second i) 5)))
              (APPLY_VELOCITY_FROM_BAR)]
             )
           
           ; now lets remove the block that has been hit
           (vector-set! struct (get-index i struct) '())]

          ; else, means it hasn't touched the block and returns i as it is
          [#t i])))

    ; by converting the vector back to a list, creates the scene with the blocks placed on it
    ; and it does it everytime the vector gets updated so it creates the scene without the block that has been touched
    (define objects (MY_FINAL_SCENE SCENE (vector->list (MAP_BALL my-struct)) BLOCK))

    ; place all not removable objects on the scene of blocks
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
                                                                   objects)))))

  ) ;; end of PLAYGROUND

;; below the settings scene is created
(define (SETTINGS)
  (define settings (text/font "SETTINGS" 50 (list-ref color-pallete (vector-ref theme-instruction 0)) #f 'decorative 'slant 'bold #f))
  (define difficulty (text/font "Difficulty:" 25 (list-ref color-pallete (vector-ref theme-instruction 0)) #f 'decorative 'normal 'bold #f))
  (define theme (text/font "Theme:" 25 (list-ref color-pallete (vector-ref theme-instruction 0)) #f 'decorative 'normal 'bold #f))
  (define music (text/font "Music:" 25 (list-ref color-pallete (vector-ref theme-instruction 0)) #f 'decorative 'normal 'bold #f))
  (define surroundings (rectangle 150 50 "outline" (list-ref color-pallete (vector-ref theme-instruction 0))))
  (define back (text/font "Go Back!" 25 (list-ref color-pallete (vector-ref theme-instruction 0)) #f 'decorative 'normal 'bold #f))
  (define right-arrow (polygon (list (make-pulled-point 1/2 10 0 0 1/2 -10)
                 (make-posn -5 10)
                 (make-pulled-point 1/2 -10 30 0 1/2 10)
                 (make-posn -5 -10))
           "solid"
           (list-ref color-pallete (vector-ref theme-instruction 0))))
  (define left-arrow (polygon (list (make-pulled-point -1/2 -10 0 0 -1/2 10)
                 (make-posn 5 -10)
                 (make-pulled-point -1/2 10 -30 0 -1/2 -10)
                 (make-posn 5 10))
           "solid"
           (list-ref color-pallete (vector-ref theme-instruction 0))))
  (define diff-level  `(,(text/font "Easy" 25 (list-ref color-pallete (vector-ref theme-instruction 0)) #f 'decorative 'normal 'bold #f)
                        ,(text/font "Medium" 25 (list-ref color-pallete (vector-ref theme-instruction 0)) #f 'decorative 'normal 'bold #f)
                        ,(text/font "Hard" 25 (list-ref color-pallete (vector-ref theme-instruction 0)) #f 'decorative 'normal 'bold #f)))
  (define theme-pack (text/font (list-ref color-pallete (vector-ref theme-instruction 0)) 25 (list-ref color-pallete (vector-ref theme-instruction 0)) #f 'decorative 'normal 'bold #f))
  (define music-level (text/font "50" 25 (list-ref color-pallete (vector-ref theme-instruction 0)) #f 'decorative 'normal 'bold #f))

  (define first-third (place-image settings
                                   250
                                   130
                                   (place-image difficulty
                                                150
                                                230
                                                (place-image theme
                                                             150
                                                             280
                                                             (place-image music
                                                                          150
                                                                          330
                                                                           (place-image surroundings
                                                                                        250
                                                                                        400
                                                                                        (place-image back
                                                                                                     250
                                                                                                     400
                                                                                                     SCENE)))))))
  (define second-third (place-image right-arrow
                                   430
                                   230
                                   (place-image right-arrow
                                                430
                                                280
                                                (place-image right-arrow
                                                             430
                                                             330
                                                             (place-image left-arrow
                                                                          250
                                                                          230
                                                                          (place-image left-arrow
                                                                                       250
                                                                                       280
                                                                                       (place-image left-arrow
                                                                                                    250
                                                                                                    330
                                                                                                    first-third)))))))
  (place-image (list-ref diff-level (vector-ref difficulty-instruction 0))
               340
               230
               (place-image theme-pack
                            340
                            280
                            (place-image music-level
                                         340
                                         330
                                         second-third)))
  ) ;; end of SETTINGS

;; lets now make the mouse events
(define (mouse-handler world x y click)
  (cond
    [(eq? (vector-ref STATE-VECTOR 0) "menu") (cond
                                                [(and (mouse=? click "button-down") 
                                                      (<= x 325) (>= x 175)
                                                      (<= y 265) (>= y 215)) (vector-set! STATE-VECTOR 0 "start")]
                                                ;; if a button mouse is clicke in the start rectangle, we send "start"
                                                ;; instruction to STATE-VECTOR to initialize PLAYGROUND
                                                [(and (mouse=? click "button-down")
                                                      (<= x 325) (>= x 175)
                                                      (<= y 325) (>= y 275)) (vector-set! STATE-VECTOR 0 "settings")]
                                                ;; if a button mouse is clicke in the start rectangle, we send "settings"
                                                ;; instruction to STATE-VECTOR to initialize SETTINGS
                                                 [(and (mouse=? click "button-down")
                                                     (<= x 325) (>= x 175)
                                                     (<= y 385) (>= y 335))  (vector-set! STATE-VECTOR 0 "Exit")]
                                           )]
    
    [(eq? (vector-ref STATE-VECTOR 0) "settings") (cond
                                                    [(and (mouse=? click "button-down")
                                                          (<= x 325) (>= x 175)
                                                          (<= y 425) (>= y 375)) (vector-set! SETTINGS_VECTOR 0 "go back")]
                                                    ;; if a button mouse is clicke in the start rectangle, we send "go back"
                                                    ;; instruction to SETTINGS_VECTOR to go back to menu
                                                    
                                                    [(and (mouse=? click "button-down")
                                                          (<= x 445) (>= x 415)
                                                          (<= y 245) (>= y 215)) (if (= (vector-ref difficulty-instruction 0) 2);; if the last element of the list
                                                                                     (vector-set! difficulty-instruction 0 0) ;; then we set the index back to 0, first one
                                                                                     (vector-set! difficulty-instruction 0 (+ (vector-ref difficulty-instruction 0) 1)))] ;; else we increase by 1
                                                    [(and (mouse=? click "button-down")
                                                          (<= x 265) (>= x 235)
                                                          (<= y 245) (>= y 215)) (if (= (vector-ref difficulty-instruction 0) 0) ;; if the first element of the list
                                                                                     (vector-set! difficulty-instruction 0 2) ;; then we set the index to the last one 
                                                                                     (vector-set! difficulty-instruction 0 (- (vector-ref difficulty-instruction 0) 1)))] ;; else we decrease by 1
                                                    [(and (mouse=? click "button-down")
                                                          (<= x 445) (>= x 415)
                                                          (<= y 295) (>= y 265)) (if (= (vector-ref theme-instruction 0) 5) 
                                                                                     (vector-set! theme-instruction 0 0) 
                                                                                     (vector-set! theme-instruction 0 (+ (vector-ref theme-instruction 0) 1)))] 
                                                    [(and (mouse=? click "button-down")
                                                          (<= x 265) (>= x 235)
                                                          (<= y 295) (>= y 265)) (if (= (vector-ref theme-instruction 0) 0)
                                                                                     (vector-set! theme-instruction 0 5)
                                                                                     (vector-set! theme-instruction 0 (- (vector-ref theme-instruction 0) 1)))]
                                                    )]
    [(eq? (vector-ref STATE-VECTOR 0) "empty") (MAIN_MENU)]
    )
  )
;; SETTINGS_VECTOR stores instructions
(define SETTINGS_VECTOR (make-vector 1))
;; STATE-VECTOR stores instructions to change between scenes
(define STATE-VECTOR (make-vector 1))
;; initial state is "menu"
(vector-set! STATE-VECTOR 0 "menu")

;; this function is drawing the image in big-bang based on the instruction stored in STATE-VECTOR
(define (WorldStates state)
  (cond
    [(eq? (vector-ref STATE-VECTOR 0) "start") (PLAYGROUND)]
    [(eq? (vector-ref STATE-VECTOR 0) "settings") (cond
                                              [(eq? (vector-ref SETTINGS_VECTOR 0) "go back") (vector-set! SETTINGS_VECTOR 0 0)
                                                                                              (vector-set! STATE-VECTOR 0 "menu")
                                                                                              (MAIN_MENU)]
                                              [#t (SETTINGS)])
                                                  ]
    [(eq? (vector-ref STATE-VECTOR 0) "menu") (MAIN_MENU)]
    )
  )

(define (should-stop? world)
  (eq? (vector-ref STATE-VECTOR 0) "exit"))
; big-bang displays the created world
(big-bang WorldStates
  (to-draw WorldStates) ; draws the image
  (on-tick UPDATE_POSITION 1/120)
  (on-key MOVE)
  (on-mouse mouse-handler)
 (stop-when should-stop?)
  )                        
