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
                       (meta (magnification 3)
                             (reset-tick 30)
                             (reset-key "r")))
}

An example frame from a presentation big-bang:

@image{example.png}.

@section{Customizing the animation}

The new @racket{meta} block in the @racket{presentation-big-bang} sets
parameters used in the animation.

The @racket{magnification} parameter controls the scale factor applied
to the draw handler.

The @racket{reset-tick} parameter sets a value at which the entire
model resets to its original state. By default this is 300.

The @racket{reset-key} is a key (string) which resets the model to
its original state when pressed. By default this key is @literal{"escape"}.


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

Originally there was a @racket{meta-draw} argument. That is no longer
exposed, but it would be easy to add.

@defproc[(meta-draw-h [md meta?] [img image?])
         image?]{
         Returns an image with the desired metadata rendered on it.
         The @racket{img} is the result returned from the plain draw handler.
         }
Providing the @racket{#:initial-metadata} argument allows a different
starting value. The values are propagated with @racket{struct-copy} so
changing the type of structure probably will not work well.

@subsection{All recognized options}

Unrecognized options are passed directly to @code{big-bang}.

@defform[#:literals (to-draw on-draw on-tick on-mouse on-key on-release on-pad on-receive check-with stop-when meta magnification reset-tick reset-key)
(presentation-big-bang INITIAL-MODEL
  [(to-draw draw-handler [width height])]
  (on-tick tick-handler [delay [tick-count]])
  (on-mouse mouse-handler)
  (on-key key-handler)
  (on-release key-handler)
  (on-pad pad-handler)
  (on-receive receive-handler)
  (check-with good-struct?)
  (stop-when should-stop? [last-scene-draw-handler])
  (meta (magnification 3)
        (reset-tick 40)
        (reset-key "r")))]{
  Produces a big-bang animation that tracks metadata.
}
