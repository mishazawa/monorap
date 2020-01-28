#lang racket
(require 2htdp/universe 2htdp/image)

(struct interval (small big))

(define LOWER 1)
(define UPPER 100)

(define (guess w)
  (define lower (interval-small w))
  (define upper (interval-big   w))
  (quotient (+ lower upper) 2))

(define (smaller w)
  (define lower (interval-small w))
  (interval lower (max lower (sub1 (guess w)))))

(define (bigger w)
  (define upper (interval-big w))
  (interval (min upper (add1 (guess w))) upper))

(define (single? w)
  (= (interval-small w) (interval-big w)))

(define WIDTH  480)
(define HEIGHT 640)

(define TEXT-SIZE    12)
(define TEXT-X       10)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y (- HEIGHT 10))
(define COLOR "red")

(define HELP-TEXT-TOP
  (text "arrow up for larger numbers, arrow down for smaller ones" TEXT-SIZE "blue"))
(define HELP-TEXT-BOTTOM
  (text "= when your number is guessed; q to quit."                TEXT-SIZE "blue"))


(define SCENE
  (place-image/align
   HELP-TEXT-TOP TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT-BOTTOM TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

(define (draw-end-fn w)
  (overlay (text "End game." TEXT-SIZE COLOR) SCENE))

(define (draw-fn w)
  (overlay (text (number->string (guess w)) TEXT-SIZE COLOR) SCENE))

(define (input-fn w key)
  (cond ((key=? key "up")   (bigger    w))
        ((key=? key "down") (smaller   w))
        ((key=? key "q")    (stop-with w))
        ((key=? key "=")    (stop-with w))
        (else w)))

(define (start lower upper)
  (big-bang (interval lower upper)
    (on-key input-fn)
    (to-draw draw-fn)
    (stop-when single? draw-end-fn)))

(start LOWER UPPER)