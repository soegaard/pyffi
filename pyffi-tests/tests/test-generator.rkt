#lang at-exp racket

(require pyffi racket/sequence)

(initialize)
(post-initialize)

(run* @~a{def f():
           x=0
           while 1:
             x=x+1
             yield x})

(run* @~a{def g():
           yield 41
           yield 42
           yield 43
           return 0})



(for/list ([_ 5] [x (main.f)]) x)
(for/list ([_ 5] [x (main.g)]) x)
