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

.. _Kawa: https://www.gnu.org/software/kawa/
.. _JLine3: https://github.com/jline/jline3
.. _javax.sound.midi: https://docs.oracle.com/javase/7/docs/api/javax/sound/midi/package-summary.html
.. _PKGBUILDs repository: https://github.com/wasamasa/pkgbuilds/
.. _example file: https://github.com/wasamasa/waka/blob/master/config
.. _Alda: https://github.com/alda-lang/alda
.. _separate file: https://github.com/wasamasa/waka/blob/master/grammar.ebnf
