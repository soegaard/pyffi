#lang racket
(require pyffi)

(initialize)
(post-initialize)

(import pygame)
;(import pygame.display as pd)
;(import pygame.image   as pi)

(pygame.init)

(define width  320)
(define height 240)
(define size   (tuple width height))

(define speed (pylist 2 2))
(define black (tuple 0 0 0))

;(define screen (pygame.display.set_mode size)) ; todo
(define screen   (pygame.display .set_mode size)) 
(define ball     (pygame.image .load "intro_ball.gif"))
(define ballrect (ball .get_rect))

(define (flip-sign i)
  (pylist-set! speed i (- (pylist-ref speed i))))

(let loop ()
  (for ([event (in-pylist (pygame.event .get))])
    (when (= event.type pygame.QUIT)
      (exit))

    (set! ballrect (ballrect .move speed))
    (when (or (< ballrect.left 0)
              (> ballrect.right width))
      (flip-sign 0))
    (when (or (< ballrect.top 0)
              (> ballrect.bottom height))
      (flip-sign 1))
    
    (screen .fill black)
    (screen .blit ball ballrect)
    (pygame.display .flip))
  (loop))


;; import sys, pygame
;; pygame.init()

;; size = width, height = 320, 240
;; speed = [2, 2]
;; black = 0, 0, 0

;; screen = pygame.display.set_mode(size)

;; ball = pygame.image.load("intro_ball.gif")
;; ballrect = ball.get_rect()

;; while 1:
;;     for event in pygame.event.get():
;;         if event.type == pygame.QUIT: sys.exit()

;;     ballrect = ballrect.move(speed)
;;     if ballrect.left < 0 or ballrect.right > width:
;;         speed[0] = -speed[0]
;;     if ballrect.top < 0 or ballrect.bottom > height:
;;         speed[1] = -speed[1]

;;     screen.fill(black)
;;     screen.blit(ball, ballrect)
;;     pygame.display.flip()
