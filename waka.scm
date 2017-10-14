#!/usr/bin/env kawa

(import (scheme base))
(import (scheme char))
(import (scheme read))
(import (scheme write))
(import (srfi 1))

(import (class gnu.text SyntaxException))
(import (class java.io File FileInputStream Reader))
(import (class javax.sound.midi
               Instrument MetaMessage MidiChannel
               MidiEvent MidiFileFormat MidiSystem
               Sequence Sequencer ShortMessage
               Soundbank Synthesizer Track))
(import (class org.jline.reader
               LineReader LineReaderBuilder
               EndOfFileException UserInterruptException))
(import (class org.jline.terminal Terminal TerminalBuilder))

;;; utils

(define (print . items)
  (for-each (lambda (item) (display item) (newline)) items))

(define (alist-ref key alist)
  (let ((match (assoc key alist)))
    (if match
        (cdr match)
        match)))

(define (identity x) x)

;;; settings

(define (xdg-path environment-variable fallback path)
  (let ((home (get-environment-variable environment-variable)))
    (if (and home (eqv? (string-ref home 0) #\/))
        (string-append home path)
        (string-append (get-environment-variable "HOME") fallback path))))

(define user-config-path
  (xdg-path "XDG_CONFIG_HOME" "/.config" "/waka/config"))

(define repl-history-path ::String
  (xdg-path "XDG_DATA_HOME" "/.local/share" "/waka/repl_history"))

(define user-config
  (if (file-exists? user-config-path)
      (with-input-from-file user-config-path read)
      '()))

(define user-base-octave (alist-ref 'base-octave user-config))
(define user-initial-mode (alist-ref 'initial-mode user-config))
(define user-instrument-id (alist-ref 'default-instrument-id user-config))
(define user-map (alist-ref 'map user-config))
(define user-prompt (alist-ref 'prompt user-config))
(define user-soundbank-path (alist-ref 'soundbank user-config))
(define user-velocity (alist-ref 'default-velocity user-config))
(define user-bpm (alist-ref 'default-bpm user-config))
(define user-quantization (alist-ref 'default-quantization user-config))

(define synthesizer #f)
(define sequencer #f)
(define channels #f)
(define channel-id 0)
(define channel #f)
(define velocity #f)
(define soundbank #f)
;; TODO: allow another customizable for instrument aliases
;; NOTE: http://www.jimmenard.com/midi_ref.html#General_MIDI
(define instruments #f)
(define instrument-id #f)
(define instrument #f)
(define default-duration 4)
(define base-octave #f)
(define resolution 16) ;; ticks per quarter
(define bpm #f)
(define quantization #f)

(define initial-mode #f)
(define terminal #f)
(define reader #f)

;;; MIDI initialization
;; adapted from http://patater.com/gbaguy/javamidi.htm

(define (init-midi!)
  (display "Initializing MIDI... ")
  (flush-output-port)

  (set! synthesizer (MidiSystem:getSynthesizer))
  (Synthesizer:open synthesizer)
  (set! sequencer (MidiSystem:getSequencer))
  (Sequencer:open sequencer)
  (let ((transmitter (Sequencer:getTransmitter sequencer))
        (receiver (Synthesizer:getReceiver synthesizer)))
    (transmitter:setReceiver receiver))

  (set! channels (Synthesizer:getChannels synthesizer))
  (set! channel (channels channel-id))
  (set! velocity (or user-velocity 64))
  (set! base-octave (or user-base-octave 4))
  (set! bpm (or user-bpm 120))
  (set! quantization (or user-quantization 1.0))

  (set! soundbank
        (if user-soundbank-path
            (MidiSystem:getSoundbank (File (as String user-soundbank-path)))
            (Synthesizer:getDefaultSoundbank synthesizer)))

  (set! instruments (Soundbank:getInstruments soundbank))
  (set! instrument-id (or user-instrument-id 0))
  (set! instrument
        (if (< instrument-id (as Instrument[] instruments):length)
            (instruments instrument-id)
            (begin
              (print "Out of bounds, falling back to instrument zero...")
              (instruments 0))))

  (Synthesizer:loadInstrument synthesizer instrument)
  ;; NOTE: without this line the instrument isn't actually used...
  ;; TODO: make it a per-channel thing
  (MidiChannel:programChange channel instrument-id)

  (print "Done!"))

(define (init-term!)
  (set! initial-mode
        (cond
         ((memq user-initial-mode '(repl free-play)) user-initial-mode)
         (user-initial-mode (print "Invalid initial mode, falling back to repl")
                            'repl)
         (else 'repl)))

  (set! terminal
        (let* ((builder (TerminalBuilder:builder))
               (builder (builder:nativeSignals #t))
               (builder (builder:signalHandler Terminal:SignalHandler:SIG_IGN)))
          (builder:build)))

  (Terminal:enterRawMode terminal)

  (set! reader
        (let* ((builder (LineReaderBuilder:builder))
               (builder (builder:variable LineReader:HISTORY_FILE
                                          (File repl-history-path)))
               (builder (builder:terminal terminal)))
          (builder:build)))

  (LineReader:unsetOpt reader LineReader:Option:HISTORY_IGNORE_SPACE)

  (print "Exit with C-d"))

;;; MIDI utils

(define (midi-note-octave note)
  (when (or (< note 0) (> note 127))
    (error "Note must be between 0 and 127 (inclusive)"))
  (- (quotient note 12) 1))

(define (midi-note->string note)
  (when (or (< note 0) (> note 127))
    (error "Note must be between 0 and 127 (inclusive)"))
  (let* ((note+sharp (case (remainder note 12)
                       ((0)  '("c"))
                       ((1)  '("c" . "+"))
                       ((2)  '("d"))
                       ((3)  '("d" . "+"))
                       ((4)  '("e"))
                       ((5)  '("f"))
                       ((6)  '("f" . "+"))
                       ((7)  '("g"))
                       ((8)  '("g" . "+"))
                       ((9)  '("a"))
                       ((10) '("a" . "+"))
                       ((11) '("b"))
                       (else (error "This shouldn't happen"))))
         (note (car note+sharp))
         (sharp (cdr note+sharp)))
    (if (null? sharp)
        note
        (string-append note sharp))))

(define (byte->midi-note byte)
  (if user-map
      (let ((mapping (assoc (integer->char byte) user-map)))
        (if mapping
            (+ (* (+ base-octave 1) 12) (cdr mapping))
            #f))
      byte))

(define (note->midi-note note octave)
  (let ((offset (case note
                  ((#\c) 0)
                  ((#\d) 2)
                  ((#\e) 4)
                  ((#\f) 5)
                  ((#\g) 7)
                  ((#\a) 9)
                  ((#\b) 11)
                  (else (error "Invalid note")))))
    (+ (* (+ octave 1) 12) offset)))

;;; parsing

(define-record-type token-port
  (make-token-port tokens)
  token-port?
  (tokens token-port-tokens token-port-tokens-set!))

(define (peek-token port)
  (let ((tokens (token-port-tokens port)))
    (if (pair? tokens)
        (car tokens)
        (eof-object))))

(define (read-token port)
  (let ((tokens (token-port-tokens port))
        (token (peek-token port)))
    (when (not (eof-object? token))
      (token-port-tokens-set! port (cdr tokens)))
    token))

(define (string->tokens input)
  (define (whitespace? char)
    (or (char-whitespace? char) (eqv? char #\|)))
  (define (read-tokens port)
    (let loop ((tokens '()))
      (let ((char (peek-char port)))
        (if (eof-object? char)
            (reverse tokens)
            (cond
             ((whitespace? char)
              (read-whitespace port)
              (loop tokens))
             ((eqv? char #\#)
              (read-line port)
              (loop tokens))
             ((eqv? char #\()
              (loop (cons (read port) tokens)))
             ((eqv? char #\))
              (error "Unexpected closing paren"))
             (else
              (loop (cons (read-token port) tokens))))))))
  (define (read-whitespace port)
    (let loop ()
      (when (whitespace? (peek-char port))
        (read-char port))))
  (define (read-token port)
    (let loop ((chars '()))
      (let ((char (peek-char port)))
        (if (and (not (eof-object? char))
                 (not (whitespace? char))
                 (not (memv char '(#\; #\())))
            (loop (cons (read-char port) chars))
            (list->string (reverse chars))))))
  (call-with-input-string input read-tokens))

(define-record-type parse-error-object
  (make-parse-error-object port token message)
  parse-error-object?
  (port parse-error-port)
  (token parse-error-token)
  (message parse-error-message))

(define (parse-error port token message)
  (raise (make-parse-error-object port token message)))

(define last-token (make-parameter #f))

(define (parse input score?)
  (define (accept-char port char)
    (if (eqv? (peek-char port) char)
        (read-char port)
        #f))
  (define (parse-score token-port)
    (let loop ((tracks '()))
      (let ((track (parse-track token-port)))
        (if track
            (loop (cons track tracks))
            (cond
             ((null? tracks)
              (error "Expected at least one track"))
             ((eof-object? (peek-token token-port))
              (reverse tracks))
             (else
              ;; TODO: read all tokens and join them
              (error "Trailing garbage" (peek-token token-port))))))))
  (define (parse-track token-port)
    (let ((name (parse-name token-port)))
      (if name
          (let ((sequence (parse-sequence token-port #f)))
            (if (not sequence)
                (error "Expected a sequence")
                sequence))
          #f)))
  (define (name-token? token)
    (and (string? token)
         (eqv? (string-ref token (- (string-length token) 1)) #\:)))
  (define (parse-name token-port)
    (if (name-token? (peek-token token-port))
        (call-with-input-string (read-token token-port) read-name)
        #f))
  (define (read-name port)
    (let loop ((chars '()))
      (let ((char (peek-char port)))
        (if (or (char-upper-case? char)
                (char-lower-case? char)
                (char-numeric? char))
            (loop (cons (read-char port) chars))
            (if (or (null? chars) (not (eqv? (peek-char port) #\:)))
                #f
                (list->string (reverse chars)))))))
  (define (parse-sequence token-port top-level?)
    (let loop ((items '()))
      (let ((item (parse-item token-port)))
        (if item
            (loop (cons item items))
            (if (null? items)
                (if top-level?
                    (error "Expected item")
                    #f)
                (reverse items))))))
  (define (parse-item token-port)
    (let ((token (peek-token token-port)))
      (cond
       ((pair? token)
        (read-token token-port)
        (cons 'sexp token))
       ((name-token? token) #f)
       ((equal? token "<")
        (read-token token-port)
        '(octave-shift . -1))
       ((equal? token ">")
        (read-token token-port)
        '(octave-shift . 1))
       ((string? token)
        (read-token token-port)
        (parameterize ((last-token token))
          (call-with-input-string token read-item)))
       (else #f))))
  (define (read-item port)
    (let ((item (or (read-chord port)
                    (read-rest port)
                    (read-octave port)
                    #f)))
      (cond
       ((eof-object? (peek-char port))
        item)
       (item
        (parse-error port (last-token) "Trailing garbage"))
       (else
        (parse-error port (last-token) "Expected item")))))
  (define (read-chord port)
    (let ((note (read-note port)))
      (if note
          (let loop ((notes '()))
            (if (accept-char port #\/)
                (let ((note (read-note port)))
                  (if note
                      (loop (cons note notes))
                      (parse-error port (last-token) "Expected note")))
                (if (pair? notes)
                    `(chord ,@(cons note (reverse notes)))
                    note)))
          #f)))
  (define (read-note port)
    (let* ((octave-shifts (read-octave-shifts port))
           (key (read-key port)))
      (if key
          (let loop ((modifiers '()))
            (let ((modifier (read-modifier port)))
              (if modifier
                  (loop (cons modifier modifiers))
                  (let ((modifiers (if octave-shifts
                                       (cons octave-shifts modifiers)
                                       modifiers)))
                    `(note (key . ,key) ,@(reverse modifiers))))))
          #f)))
  (define (read-key port)
    (if (memv (peek-char port) '(#\a #\b #\c #\d #\e #\f #\g))
        (read-char port)
        #f))
  (define (read-modifier port)
    (cond
     ((read-duration port) => (lambda (duration) `(duration . ,duration)))
     ((read-accidentals port) => identity)
     ((read-natural port) '(natural . #t))
     ((read-dotted port) => identity)
     (else #f)))
  (define (read-accidentals port)
    (let loop ((accidentals '()))
      (let ((char (peek-char port)))
        (case char
          ((#\+) (read-char port) (loop (cons 1 accidentals)))
          ((#\-) (read-char port) (loop (cons -1 accidentals)))
          (else
           (if (null? accidentals)
               #f
               `(shift . ,(fold + 0 accidentals))))))))
  (define (read-natural port)
    (if (accept-char port #\_)
        #t
        #f))
  (define (read-dotted port)
    (let loop ((count 0))
      (if (accept-char port #\.)
          (loop (+ count 1))
          (if (zero? count)
              #f
              `(dotted . ,count)))))
  (define (read-octave-shifts port)
    (let loop ((shifts '()))
      (let ((char (peek-char port)))
        (case char
          ((#\<) (read-char port) (loop (cons -1 shifts)))
          ((#\>) (read-char port) (loop (cons 1 shifts)))
          (else
           (if (null? shifts)
               #f
               `(octave-shift . ,(fold + 0 shifts))))))))
  (define (read-rest port)
    (if (accept-char port #\r)
        (let ((duration (read-duration port)))
          (if duration
              `(rest . ,duration)
              (if (eof-object? (peek-char port))
                  '(rest)
                  (error "Invalid duration"))))
        #f))
  (define (read-octave port)
    (if (accept-char port #\o)
        (let ((octave (read-digit port)))
          (if octave
              `(octave . ,octave)
              (parse-error port (last-token) "Invalid octave")))
        #f))
  (define (digit? char)
    (memv char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
  (define (valid-duration? duration)
    (memv duration '(1 2 4 8 16 32 64)))
  (define (read-duration port)
    (let ((duration (read-digits port)))
      (if duration
          (let loop ((durations '()))
            (if (accept-char port #\~)
                (let ((duration (read-digits port)))
                  (if duration
                      (loop (cons duration durations))
                      (parse-error port (last-token) "Expected duration")))
                (if (pair? durations)
                    (let ((durations (cons duration (reverse durations))))
                      (for-each
                       (lambda (duration)
                         (when (not (valid-duration? duration))
                           (error "Invalid duration" duration)))
                       durations)
                      durations)
                    (if (valid-duration? duration)
                        duration
                        (error "Invalid duration" duration)))))
          #f)))
  (define (read-digits port)
    (let loop ((chars '()))
      (let ((char (peek-char port)))
        (if (digit? char)
            (loop (cons (read-char port) chars))
            (if (null? chars)
                #f
                (string->number (list->string (reverse chars))))))))
  (define (read-digit port)
    (let ((char (peek-char port)))
      (if (digit? char)
          (- (char->integer (read-char port)) 48)
          #f)))

  (let* ((tokens (string->tokens input))
         (token-port (make-token-port tokens)))
    (when (null? tokens)
      (error "Empty input"))
    (if score?
        (parse-score token-port)
        (list (parse-sequence token-port #t)))))

;; adapted from
;; http://archive.oreilly.com/pub/a/onjava/excerpt/jenut3_ch17/index1.html

(define (sequence->midi tracks)
  (define (duration->ticks duration #!optional dotted)
    (let ((ticks (if (pair? duration)
                     (durations->ticks duration)
                     (durations->ticks (list duration)))))
      (if dotted
          (exact (round (* ticks (- 2 (/ 1 (expt 2 dotted))))))
          ticks)))
  (define (durations->ticks durations)
    (fold + 0 (map (lambda (duration) (/ (* resolution 4) duration))
                   durations)))
  (define (add-note-on track start key velocity)
    (let ((note (ShortMessage)))
      (note:setMessage ShortMessage:NOTE_ON channel-id key velocity)
      (Track:add track (MidiEvent note start))))
  (define (add-silent-note track start key)
    (add-note-on track start key 0))
  (define (add-note track start length key velocity)
    (add-note-on track start key velocity)
    (let ((end (+ start (exact (round (* quantization length))))))
      (add-silent-note track end key)))
  (let ((sequence (Sequence Sequence:PPQ resolution)))
    (for-each
     (lambda (sexps)
       (let ((track (sequence:createTrack))
             (last-dotted #f))
         ;; TODO: send instrument change message for multi-instrument support
         ;; NOTE: changing the instrument seems to only work for the synth
         ;; or a channel, so you'd need to stop hardcoding channel 0,
         ;; start using more than one channel and treat channel 9 specially
         ;; NOTE: consider changing track name syntax to instrument number
         ;; (how would you do percussion then?)
         (let loop ((sexps sexps)
                    (t 0)
                    (last-duration default-duration))
           (when (pair? sexps)
             (let* ((sexp (car sexps))
                    (type (car sexp))
                    (value (cdr sexp)))
               (case type
                 ((note chord)
                  ;; TODO: support octave shift modifier
                  ;; TODO: support natural modifier (after supporting key
                  ;; signature)
                  (let ((values (if (eq? type 'note)
                                    (list value)
                                    (map cdr value)))
                        ;; NOTE: octave shifts in chords shouldn't
                        ;; affect notes after the chord
                        (chord-octave base-octave)
                        (chord-ticks #f)
                        (chord-duration #f))
                    (for-each
                     (lambda (value)
                       (let ((octave-shift (alist-ref 'octave-shift value)))
                         (when octave-shift
                           (set! chord-octave (+ chord-octave octave-shift))))
                       ;; TODO: support rests in chords
                       ;; NOTE: https://github.com/alda-lang/alda/blob/master/doc/chords.md
                       ;; explains why you'd want that
                       (let* ((duration (alist-ref 'duration value))
                              (dotted (alist-ref 'dotted value))
                              (shift (or (alist-ref 'shift value) 0))
                              (note (note->midi-note (alist-ref 'key value)
                                                     chord-octave)))
                         ;; NOTE: reset dotted modifier when given a new
                         ;; duration, otherwise reuse (last) dotted value
                         (when duration
                           (set! last-dotted #f))
                         (let* ((duration (or duration last-duration))
                                (dotted (or dotted last-dotted))
                                (ticks (duration->ticks duration dotted)))
                           (add-note track t ticks (+ note shift) velocity)
                           (set! last-dotted dotted)
                           (if chord-ticks
                               (when (< ticks chord-ticks)
                                 (set! chord-ticks ticks)
                                 (set! chord-duration duration))
                               (begin
                                 (set! chord-ticks ticks)
                                 (set! chord-duration duration))))))
                     values)
                    (loop (cdr sexps) (+ t chord-ticks) chord-duration)))
                 ((rest)
                  (let* ((duration (if (null? value)
                                       last-duration
                                       value))
                         (ticks (duration->ticks duration)))
                    ;; HACK: ensure final rest isn't cut off
                    ;; NOTE: this will only work if the rest is the last
                    ;; thing in the score
                    (when (null? (cdr sexps))
                      (add-silent-note track (+ t ticks) 0))
                    (loop (cdr sexps) (+ t ticks) duration)))
                 ((octave)
                  (set! base-octave value)
                  (loop (cdr sexps) t last-duration))
                 ((octave-shift)
                  (if (positive? value)
                      (when (< base-octave 9)
                        (set! base-octave (+ base-octave 1)))
                      (when (>= base-octave 0)
                        (set! base-octave (- base-octave 1))))
                  (loop (cdr sexps) t last-duration))
                 ((sexp)
                  (let ((type (car value))
                        (value (cdr value)))
                    (case type
                      ;; TODO: instrument (create instrument change procedure)
                      ;; TODO: sustain (implement that feature first)
                      ;; TODO: dampening pedal (see above)
                      ;; TODO: allow setting midi metadata with sexps
                      ;; TODO: per track vs global settings (velocity?)
                      ((velocity)
                       (if (null? value)
                           (print velocity)
                           (set! velocity (car value))))
                      ((tempo bpm)
                       (if (null? value)
                           (print bpm)
                           (set! bpm (car value))))
                      ((quant quantize quantization)
                       (if (null? value)
                           (print quantization)
                           (set! quantization (car value))))
                      (else
                       (error "Unknown sexp type" type)))
                    (loop (cdr sexps) t last-duration)))
                 (else
                  (error "Unimplemented item type" type))))))))
       tracks)
     sequence))

;;; play modes

(define (free-play)
  (define (display-octave octave)
    (display "o")
    (display octave)
    (display " ")
    (flush-output-port))
  (print "Free play mode entered, toggle with C-SPC")
  (display-octave base-octave)
  (let ((reader (Terminal:reader terminal))
        (last-octave base-octave))
    (let loop ()
      (let ((byte (Reader:read reader)))
        (case byte
          ((4) (newline) #f) ; EOF
          ((10 13) (newline) (display-octave last-octave) (loop)) ; CR/LF
          ((0) (newline) (repl)) ; C-SPC
          ((60) ; <
           (when (> base-octave -1)
             (set! base-octave (- base-octave 1))
             (set! last-octave base-octave)
             (display "< ")
             (flush-output-port))
           (loop))
          ((62) ; >
           (when (< base-octave 9)
             (set! base-octave (+ base-octave 1))
             (set! last-octave base-octave)
             (display "> ")
             (flush-output-port))
           (loop))
          (else
           (let* ((midi-note (byte->midi-note byte))
                  (octave (and midi-note (midi-note-octave midi-note))))
             (when midi-note
               (MidiChannel:noteOn channel midi-note velocity)
               (cond
                ((> octave last-octave)
                 (display (string-repeat "> " (- octave last-octave))))
                ((< octave last-octave)
                 (display (string-repeat "< " (- last-octave octave)))))
               (display (midi-note->string midi-note))
               (display " ")
               (flush-output-port)
               (set! last-octave octave)))
           (loop)))))))

(define (repl)
  (print "REPL mode entered, toggle with C-c, cancel input with C-g")
  (let ((prompt ::String (or user-prompt "midi> ")))
    (let loop ()
      (let ((input (try-catch
                    (LineReader:readLine reader prompt)
                    (e UserInterruptException (free-play) #f)
                    (e EndOfFileException #f))))
        (when input
          (guard
           (ex ((parse-error-object? ex)
                (display "Error: ")
                (print (parse-error-message ex))
                (let* ((token (parse-error-token ex))
                       (indent (port-column (parse-error-port ex)))
                       (width (string-length token)))
                  (print token)
                  (display (make-string indent #\space))
                  (display (make-string (max (- width indent) 1) #\^))
                  (newline)
                  (loop)))
               ((read-error? ex)
                (display "Error: ")
                (display (SyntaxException:getMessage ex))
                (flush-output-port)
                (loop))
               ((and (error-object? ex)
                     (equal? (error-object-message ex) "Empty input"))
                (loop))
               ((error-object? ex)
                (display "Error: ")
                (display (error-object-message ex))
                (if (pair? (error-object-irritants ex))
                    (begin
                      (display ": ")
                      (apply print (error-object-irritants ex)))
                    (newline))
                (loop)))
           (let ((sequence (parse input #f)))
             (Sequencer:setSequence sequencer (sequence->midi sequence))
             (Sequencer:setTempoInBPM sequencer bpm)
             (Sequencer:start sequencer))
           (loop)))))))

(define (run!)
  (if (eq? initial-mode 'repl)
      (repl)
      (free-play)))

(define (quit!)
  ;; NOTE: omitting this makes it hang...
  (Sequencer:close sequencer))

(define (midi-file? path)
  (let ((magic (call-with-input-file path
                 (lambda (in) (read-string 4 in)))))
    (equal? magic "MThd")))

(define (play-file! path)
  (if (midi-file? path)
      (play-midi-file! path)
      (play-waka-file! path)))

(define (play-midi-file! (path ::String))
  (display "Playing MIDI file... ")
  (flush-output-port)
  (let ((stream (FileInputStream path)))
    (play-midi!
     (lambda ()
       (Sequencer:setSequence sequencer stream)
       (Sequencer:setTempoInBPM sequencer bpm)))))

(define (play-waka-file! path)
  (display "Playing Waka file... ")
  (flush-output-port)
  (let* ((input (path-data path))
         (sequence (parse input #t))
         (midi (sequence->midi sequence)))
    (play-midi!
     (lambda ()
       (Sequencer:setSequence sequencer midi)
       (Sequencer:setTempoInBPM sequencer bpm)))))

(define END-OF-TRACK 47)

(define (play-midi! sequence-thunk)
  ;; NOTE: playing a MIDI sequence is asynchronous, so delay quit!
  ;; until encountering an end-of-track message
  (let ((done (promise)))
    (sequence-thunk)
    (Sequencer:addMetaEventListener
     sequencer
     (lambda (message)
       (when (= (MetaMessage:getType message) END-OF-TRACK)
         (promise-set-value! done #t)
         (quit!))))
    (Sequencer:start sequencer)
    (force done))
  (print "Done!"))

(define (multi-track-sequence? sequence)
  (> (length sequence) 1))

(define (record-midi! in-path (out-path ::String))
  (when (midi-file? in-path)
    (print "Recording MIDI to MIDI, aborting")
    (exit 1))
  (display "Recording MIDI file... ")
  (flush-output-port)
  (let* ((input (path-data in-path))
         (sequence (parse input #t))
         (midi (sequence->midi sequence))
         (version (if (multi-track-sequence? sequence) 1 0)))
    (MidiSystem:write midi version (File out-path)))
  (print "Done!"))

;;; entry point

(define usage
  (let ((program (path-last (car (command-line)))))
    (format "usage:\n  ~a # repl\n  ~a <infile> # batch play\n  ~a <infile> <outfile> # batch write"
            program program program)))

(if (get-environment-variable "RUN_TESTS")
    (include "test.scm")
    (let ((argv (cdr (command-line))))
      (cond
       ((member argv '(("--help") ("-h")))
        (print usage))
       ((> (length argv) 2)
        (print usage)
        (exit 1))
       (else
        (init-midi!)
        (cond
         ((null? argv)
          (init-term!)
          (run!)
          (quit!))
         ((= (length argv) 1)
          (play-file! (car argv)))
         ((= (length argv) 2)
          (apply record-midi! argv)
          (quit!)))))))

;; TODO: add subsequence syntax (like { ... })
;; TODO: add syntax for repeating notes/subsequences
;; TODO: check other alda syntax that's worth implementing (like
;; duration in ms/s)
;; TODO: add auto-completion for sexps
;; TODO: add debug mode
