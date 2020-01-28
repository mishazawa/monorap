#lang racket
(require 2htdp/universe 2htdp/image)

;; --------- constants ----------
(define WIDTH  100)
(define HEIGHT 100)
(define FRAMERATE 1/5)
(define FIELD-SIZE 10)
(define DEFAULT-EXPIRATION 15)
(define MAIN-SCENE (empty-scene WIDTH HEIGHT "transparent"))
(define SEGMENT-SIZE 10)
(define IMG-SEGMENT (bitmap "segment.png"))
(define IMG-SNAKE-HEAD-UP (bitmap "up-res.png"))
(define IMG-SNAKE-HEAD-DOWN (bitmap "down-res.png"))
(define IMG-SNAKE-HEAD-LEFT (bitmap "left-res.png"))
(define IMG-SNAKE-HEAD-RIGHT (bitmap "right-res.png"))
(define IMG-FOOD (bitmap "food-res.png"))

;; --------- structs   ----------

(struct position (x y)                #:transparent)
(struct game     (snake foods)        #:transparent)
(struct snake    (direction segments) #:transparent)
(struct food     (location expire)    #:transparent)

;; --------- methods   ----------

;; --- snake ---

(define (snake-head sn)
  (first (snake-segments sn)))

(define (snake-body sn)
  (rest (snake-segments sn)))

(define (snake-remove-tail segs)
  (cond ((empty? (rest segs)) '())
        (else (cons (first segs) (snake-remove-tail (rest segs))))))

(define (snake-grow sn)
  (snake (snake-direction sn)
         (cons (next-head sn) (snake-segments sn))))

(define (snake-slither sn)
  (snake (snake-direction sn)
         (cons (next-head sn) (snake-remove-tail (snake-segments sn)))))

(define (snake-change-direction sn dir)
  (snake dir (snake-segments sn)))

(define (snake-wall-collide? sn)
  (define pos (snake-head sn))
  (define x (position-x pos))
  (define y (position-y pos))
  (or (< x 0)
      (< y 0)
      (> x FIELD-SIZE)
      (> y FIELD-SIZE)))

(define (snake-self-collide? head segs)
  (cons? (member head segs)))
;; --- position ---

(define (position-move curr dx dy)
  (position (+ (position-x curr) dx)
            (+ (position-y curr) dy)))

(define (position-rand)
  (let ((x (random 0 FIELD-SIZE))
        (y (random 0 FIELD-SIZE)))
    (position x y)))

;; --- food ---

(define (food-rotten? f)
  (zero? (food-expire f)))

(define (food-decay f)
  (food (food-location f) (sub1 (food-expire f))))

(define (food-renew f segs)
  (define live-food (filter (λ (f) (not (food-rotten? f))) f))
  (if (empty? live-food)
      (list (food-gen segs))
      live-food))

(define (food-fresh pos)
  (food pos DEFAULT-EXPIRATION))

(define (food-gen snake-segs)
  (food-fresh (gen-rand-position-wo-snake-collision snake-segs)))

;; ---------- misc -----------------
;; ---------- world ----------------

(define (gen-rand-position-wo-snake-collision segs)
  (define maybe-pos (position-rand))
  (if (member maybe-pos segs)
      (gen-rand-position-wo-snake-collision segs)
      maybe-pos))

(define (init-state)
  (define snake-inst (snake "right" (list (position 1 1))))
  (game snake-inst (list (food-gen (snake-segments snake-inst)))))

(define (dead? w)
  (define sn (game-snake w))
  (or (snake-self-collide? (snake-head sn) (snake-body sn))
      (snake-wall-collide? sn)))

(define (close? head food)
  (equal? head (food-location food)))

(define (can-eat snake foods)
  (if (empty? foods)
      foods
      (if (close? (snake-head snake) (first foods))
          (first foods)
          (list))))

(define (age-food foods segs) (food-renew (map food-decay foods) segs))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir  (snake-direction sn))
  (cond ((string=? dir "up")   (position-move head  0 -1))
        ((string=? dir "down") (position-move head  0  1))
        ((string=? dir "left") (position-move head -1  0))
        ((string=? dir "right") (position-move head 1  0))))

(define (dir? k)
  (or (key=? k "up")
      (key=? k "down")
      (key=? k "left")
      (key=? k "right")))

(define (opposite-direction? a b)
  (or (and (string=? a "up")    (string=? b "down"))
      (and (string=? a "down")  (string=? b "up"))
      (and (string=? a "left")  (string=? b "right"))
      (and (string=? a "right") (string=? b "left"))))

(define (change-direction w d)
  (define sn (game-snake w))
  (if (opposite-direction? (snake-direction sn) d)
      w
      (game (snake-change-direction sn d) (game-foods w))))


;; ----------------- big bang -----------------

(define (update-fn w)
  (define snake (game-snake w))
  (define foods (game-foods w))
  (define food-to-eat (can-eat snake foods))
  (cond ((empty? food-to-eat)
         (define new-snake (snake-slither snake))
         (define new-foods (age-food foods (snake-segments new-snake)))
         (game new-snake new-foods))
        (else
         (define new-snake (snake-grow snake))
         (define new-foods (list (food-gen (snake-segments snake))))
         (game new-snake new-foods))))


(define (input-fn w ke)
  (cond ((dir? ke) (change-direction w ke))
        (else w)))

(define (img+scene seg img scene)
  (place-image img
               (* (position-x seg) SEGMENT-SIZE)
               (* (position-y seg) SEGMENT-SIZE)
               scene))

(define (img-list+scene segs img scene)
  (foldr (λ (p s) (img+scene p img s)) scene segs))

(define (snake+scene sn scene)
  (define snake-body-scene
    (img-list+scene (snake-body sn) IMG-SEGMENT scene))
  (define dir (snake-direction sn))
  (img+scene (snake-head sn)
             (cond ((string=? dir "up")    IMG-SNAKE-HEAD-UP)
                   ((string=? dir "down")  IMG-SNAKE-HEAD-DOWN)
                   ((string=? dir "left")  IMG-SNAKE-HEAD-LEFT)
                   ((string=? dir "right") IMG-SNAKE-HEAD-RIGHT))
             snake-body-scene))

(define (foods+scene fs scene)
  (img-list+scene (map food-location fs) IMG-FOOD scene))

(define (draw-fn w)
  (snake+scene (game-snake w)
               (foods+scene (game-foods w) MAIN-SCENE)))

(define (draw-end-fn w)
  (overlay (text "Game Over=(" 12 "black") (draw-fn w)))

(define (start)
  (big-bang (init-state)
    (on-tick update-fn FRAMERATE)
    (on-key  input-fn)
    (to-draw draw-fn)
    (stop-when dead? draw-end-fn)))

(start)