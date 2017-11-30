waka
====

About
-----

``waka`` is a MIDI REPL for composing music interactively.  It's built
upon Kawa_, JLine3_ and the javax.sound.midi_ package from Java SE.

Setup
-----

You'll need to install the above dependencies first and make sure your
Kawa installation has been built with JLine3 support.  If you're using
Arch Linux, you can build and install the ``kawa-git`` and
``java-jline3`` packages from my `PKGBUILDs repository`_.  Copy
``waka.scm`` to a location on your ``PATH`` and you're all set to go!

Usage
-----

Run ``waka`` without any arguments to enter the REPL.  It supports a
"free play" mode where pressing a key will play a note and a REPL mode
where you enter a sequence of notes and play them by pressing the
``Enter`` key.  Toggling is done by pressing ``C-SPC`` in free play
mode and ``C-c`` in REPL mode, to exit press ``C-d``.  The grammar for
sequences is adapted from Alda_ and documented in a `separate file`_.

The settings can be customized by placing a file at
``$XDG_CONFIG_HOME/waka/config`` or if its unset,
``~/.config/waka/config``.  An `example file`_ containing all
customizables is available in the repository.  Its key map places the
notes from C4 to E6 on the four QWERTY rows:

.. code::

    |     | c5# | d5# |     | f5# | g5# | a5# |     | c6# | d6# |
    |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  0  |
       | c5  | d5  | e5  | f5  | g5  | a5  | b5  | c6  | d6  | e6  |
       |  q  |  w  |  e  |  r  |  t  |  y  |  u  |  i  |  o  |  p  |
         |     | c4# | d4# |     | f4# | g4# | a4# |     |     |
         |  a  |  s  |  d  |  f  |  g  |  h  |  j  |  k  |  l  |
            | c4  | d4  | e4  | f4  | g4  | a4  | b4  |
            |  z  |  x  |  c  |  v  |  b  |  n  |  m  |

If you're experiencing issues such as static noise under Pulseaudio,
you can temporarily suspend it with ``pasuspender -- env PULSE_SERVER=
waka``.

Language
--------

This is an informal summary of waka's grammar.  REPL mode accepts
sequences whereas batch mode expects a superset of them known as a
score.

Sequence
........

A sequence consists of (usually, but not always space-separated)
items, the simplest of which is a note.  Other supported item types
are chords (which themselves consist of notes), rests, octave changes,
octave shifts and s-expressions.  The following plays the chromatic
scale:

.. code::

   c1 d e f g a b

To modify how a note is played, append characters to it.  A number is
interpreted as a duration measured in fractions of a whole note, with
the default duration being one-fourth.  Once set a duration is used
for the subsequent notes until a new duration is specified this way:

.. code::

   c1 e g c2 e g c4 e g

Durations themselves can be lengthened by appending one or more dots.
Each dot increases the length of the note by 50%.

.. code::

   c2. c4

Durations can be concatenated by using a tilde.  An alternative way of
specifying ``c2..`` would be:

.. code::

   c2~4~8

Accidentals change the pitch by a semitone.  ``+`` increases and
``-`` decreases it.  Much like with dots, an arbitrary amount can be
chained together.

.. code::

   c+ d+ f+ g+ a+

Notes can be combined to a chord by concatenating them with a slash:

.. code::

   c1/e/g c/e-/g

To change octaves in a chord, precede the note with as many ``<`` or
``>`` needed.  ``<`` shifts down, ``>`` shifts up.

.. code::

   a1/>c/e e/c/<a

The octave shift syntax can be used on its own to globally change the
octave:

.. code::

   a1 > c e c < a

The octave can be set with ``o`` to an absolute value:

.. code::

   o0 c1 o2 c o4 c o6 c o8 c

Rests introduce a pause and use the same duration syntax as notes:

.. code::

   r1~2~4 r4

Bars are considered whitespace

.. code::

   c1 | c2 c | c4 c c c

The hash starts a line comment:

.. code::

   # ignore this

S-expressions can represent all kinds of things.  The convention is
to treat them like Scheme parameters or in other words, ``(foo)``
returns the current value of ``foo`` whereas ``(foo bar)`` sets
the value of ``foo`` to ``bar``.  Currently recognized s-expressions:

.. code::

   (velocity 127) # global velocity: 0 - 127
   (tempo 180) # global speed in bpm
   (bpm 180) # tempo alias
   (quant 0.9) # fraction of a note to be played: 0.0 - 1.0
   (quantize 0.9) # quant alias
   (quantization 0.9) # quant alias
   (instrument trumpet) # current instrument, see instruments.scm

Score
.........

A score is a list of sequences, each preceded by a name suffixed by a
colon.  Every sequence is played on a separate channel.  The name
determines what instrument is used for the associated sequence.

.. code::

   piano: o4 c d e f g a b
   trumpet: o3 c d e f g a b

If you want to use the same instrument for more than one channel, you
can append a nickname and another colon to the name.

.. code::

   piano:main:    o4 c d e f g a b
   piano:backing: o3 c d e f g a b

Scores can be split up into interleaved parts for easier editing.
Make sure the names match up, otherwise they cannot be combined
successfully:

.. code::

   piano:main:    o4 c d e f
   piano:backing: o3 c d e f
   piano:main:    o4 g a b > c
   piano:backing: o3 g a b > c

Debugging
---------

While listening carefully to the notes is the easiest way to spot
mistakes, it may not be sufficient if you aren't sure about whether
the right notes have been generated.  The bundled ``waka2ly`` script
leverages Lilypond to generate a file that can be typeset with
``lilypond`` for visual debugging.

.. _Kawa: https://www.gnu.org/software/kawa/
.. _JLine3: https://github.com/jline/jline3
.. _javax.sound.midi: https://docs.oracle.com/javase/7/docs/api/javax/sound/midi/package-summary.html
.. _PKGBUILDs repository: https://github.com/wasamasa/pkgbuilds/
.. _example file: https://github.com/wasamasa/waka/blob/master/config
.. _Alda: https://github.com/alda-lang/alda
.. _separate file: https://github.com/wasamasa/waka/blob/master/grammar.ebnf
