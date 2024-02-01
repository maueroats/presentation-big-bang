#lang scribble/manual

@title{Presentation Big Bang}

The presentation big-bang system provides a magnified image and visual
indications showing the location of the mouse, the last key or mouse
event, and the passing of time.  It is designed to be used in
presentations or recordings to make it clear what actions are
triggering the visual responses.

@section{An Example}

@codeblock[#:keep-lang-line? #f]{
#lang racket
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
}

An example frame from a presentation big-bang:

@image{example.png}.

@section{Customizing the animation}

The @racket{#:reset-tick} parameter sets a value at which the entire
model resets to its original state. By default this is 300.

The @racket{#:reset-key} is a key (string) which resets the model to
its original state when pressed. By default this key is the back tick
(@literal{`}).

@section{Metadata}

Metadata tracks the time passing as well as the last event (keypress
or mouse event). The default metadata display shows this information
in the upper left and right of the screen.

@subsection{Information available}

 The meta struct holds information about the animation. The fields are
 the x and y coordinates of the mouse, the last event (key press or
 mouse event), and the number of ticks since the animation started.

@defstruct[meta ([x number?] [y number?] [event string?] [ticks number?])]{
 A structure that holds the metadata for an animation.
 }


@subsection{Customizing the metadata}

The way the metadata is displayed can be controlled by specifying a
@racket{#:meta-draw} argument.

@defproc[(meta-draw-h [md meta?] [img image?])
         image?]{
         Returns an image with the desired metadata rendered on it. 
         The @racket{img} is the result returned from the plain draw handler.
         }
Providing the @racket{#:initial-metadata} argument allows a different
starting value. The values are propagated with @racket{struct-copy} so
changing the type of structure probably will not work well.

@;{
@subsection{All recognized options}

Unrecognized options are passed directly to @code{big-bang}. However,
the underlying model being used is not exported, so it will be hard to
use other handlers.


@defform[#:literals (to-draw on-draw on-tick on-mouse on-key)
(presentation-big-bang [INITIAL-MODEL any?]
  [(to-draw draw-handler)]
  (on-tick tick-handler [delay])
  (on-mouse mouse-handler)
  (on-key key-handler)
  #:magnification 3
  #:reset-tick 40
  #:reset-key "r"
  #:initial-metadata (meta 0 0 "" 0)
  #:meta-draw meta-draw-h)
  #:contracts ([meta-draw-h (meta? image? -> image?)])]
  {Unfinished}
  }
  