# Presentation Big Bang

The presentation big-bang system provides a magnified image and visual
indications showing the location of the mouse, the last key or mouse
event, and the passing of time. It is designed to be used in
presentations or recordings to make it clear what actions are
triggering the visual responses.

## An Example

```racket
(require 2htdp/universe)
(require presentation-big-bang)
(define (dr n)
  (overlay (text (number->string n) 48 "black")
           (empty-scene 300 200)))
(presentation-big-bang 10
                       (to-draw dr)
                       (on-tick add1 1.0)
                       #:magnification 3
                       #:reset-tick 30
                       #:reset-key "r")
```

An example frame from a presentation big-bang:

![Magnified big-bang with annotations](example.png)
