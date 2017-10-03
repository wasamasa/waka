#!/usr/bin/env kawa

(import (scheme base))
(import (scheme char))
(import (scheme read))
(import (scheme write))
(import (srfi 1))

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

;;; settings and imports

(define user-config-path
  (let ((data-home (get-environment-variable "XDG_CONFIG_HOME")))
    (if (and data-home (eqv? (string-ref data-home 0) #\/))
        (string-append data-home "/waka/config")
        (string-append (get-environment-variable "HOME")
                       "/.config/waka/config"))))

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

(define synthesizer #f)
(define sequencer #f)
(define channels #f)
(define channel-id 0)
(define channel #f)
(define velocity #f)
(define soundbank #f)
(define instruments #f)
(define instrument-id #f)
(define instrument #f)
(define default-duration 4)
(define base-octave #f)

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
  ;; without this line the instrument isn't actually used...
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
               (builder (builder:terminal terminal)))
          (builder:build)))

  (print "Exit with C-d"))

;;; MIDI utils

(define (midi-note->string note)
  (when (or (< note 0) (> note 127))
    (error "Note must be between 0 and 127 (inclusive)"))
  (let* ((octave (- (quotient note 12) 1))
         (note+sharp (case (remainder note 12)
                       ((0)  '("c"))
                       ((1)  '("c" . "#"))
                       ((2)  '("d"))
                       ((3)  '("d" . "#"))
                       ((4)  '("e"))
                       ((5)  '("f"))
                       ((6)  '("f" . "#"))
                       ((7)  '("g"))
                       ((8)  '("g" . "#"))
                       ((9)  '("a"))
                       ((10) '("a" . "#"))
                       ((11) '("b"))
                       (else (error "This shouldn't happen"))))
         (note (car note+sharp))
         (sharp (cdr note+sharp)))
    (if (null? sharp)
        (string-append note (number->string octave))
        (string-append note (number->string octave) sharp))))

(define (byte->midi-note byte)
  (if user-map
      (let ((mapping (assoc (integer->char byte) user-map)))
        (if mapping
            (+ (* (+ base-octave 1) 12) (cdr mapping))
            #f))
      byte))

(define (note->midi-note note octave #!optional sharp?)
  (let* ((offset (case note
                   ((#\c) 0)
                   ((#\d) 2)
                   ((#\e) 4)
                   ((#\f) 5)
                   ((#\g) 7)
                   ((#\a) 9)
                   ((#\b) 11)
                   (else (error "Invalid note"))))
         (midi-note (+ (* (+ octave 1) 12) offset)))
    (if sharp?
        (+ midi-note 1)
        midi-note)))

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
             ((eqv? char #\;)
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
       ((string? token)
        (read-token token-port)
        (parameterize ((last-token token))
          (call-with-input-string token read-item)))
       (else #f))))
  (define (read-item port)
    (let ((item (or (read-chord port)
                    (read-rest port)
                    (read-octave port)
                    (read-octave-shift port)
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
    (let ((key (read-key port)))
      (if key
          (let loop ((modifiers '()))
            (let ((modifier (read-modifier port)))
              (if modifier
                  (loop (cons modifier modifiers))
                  `(note (key . ,key) ,@(reverse modifiers)))))
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
     ((read-octave-shifts port) => identity)
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
  (define (read-octave-shift port)
    (let ((char (peek-char port)))
      (case char
        ((#\<) (read-char port) '(octave-shift . -1))
        ((#\>) (read-char port) '(octave-shift . 1))
        (else #f))))
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
                (string->number (list->string chars)))))))
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
  (define (duration->ticks duration)
    (if (pair? duration)
        (durations->ticks duration)
        (durations->ticks (list duration))))
  (define (durations->ticks durations)
    (fold + 0 (map (lambda (duration) (/ 64 duration)) durations)))
  (define (add-note track start length key velocity)
    (let ((on (ShortMessage))
          (off (ShortMessage)))
      (on:setMessage ShortMessage:NOTE_ON channel-id key velocity)
      (off:setMessage ShortMessage:NOTE_OFF channel-id key velocity)
      (Track:add track (MidiEvent on start))
      (Track:add track (MidiEvent off (+ start length)))))
  ;; TODO: support multiple tracks
  (when (> (length tracks) 1)
    (error "Multi-track scores not supported yet"))
  (let* ((sequence (Sequence Sequence:PPQ 16))
         (track (sequence:createTrack)))
    ;; TODO: send instrument change message for multi-voice support
    ;; NOTE: changing the instrument seems to only work for the synth
    ;; or a channel, so you'd need to stop hardcoding channel 0 and
    ;; treat channel 9 specially
    ;; NOTE: consider changing track name syntax to instrument number
    ;; (how would you do percussion then?)
    (let loop ((sexps (car tracks))
               (t 0)
               (last-duration default-duration))
      (when (pair? sexps)
        (let* ((sexp (car sexps))
               (type (car sexp))
               (value (cdr sexp)))
          ;; TODO: support more types
          (case type
            ((note)
             ;; TODO: support more modifiers
             (let* ((duration (or (alist-ref 'duration value) last-duration))
                    (ticks (duration->ticks duration))
                    (note (note->midi-note (alist-ref 'key value) base-octave)))
               (add-note track t ticks note velocity)
               (loop (cdr sexps) (+ t ticks) duration)))
            ((rest)
             (let* ((duration (or (alist-ref 'duration value) last-duration))
                    (ticks (duration->ticks duration)))
               (loop (cdr sexps) (+ t ticks) duration)))
            ((octave)
             (set! base-octave value)
             (loop (cdr sexps) t last-duration))
            (else
             ;; ignore sexp
             (loop (cdr sexps) t last-duration))))))
    sequence))

;;; play modes

(define (free-play)
  (print "Free play mode entered, toggle with C-SPC")
  (let ((reader (Terminal:reader terminal)))
    (let loop ()
      (let ((byte (Reader:read reader)))
        (case byte
          ((4) (newline) #f) ; EOF
          ((10 13) (newline) (loop)) ; CR/LF
          ((0) (newline) (repl)) ; C-SPC
          (else
           (let ((midi-note (byte->midi-note byte)))
             (when midi-note
               (MidiChannel:noteOn channel midi-note velocity)
               (display (midi-note->string midi-note))
               (display " ")
               (flush-output-port)))
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
                  (newline)))
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
                    (newline))))
           (let ((sequence (parse input #f)))
             (print sequence)
             (Sequencer:setSequence sequencer (sequence->midi sequence))
             ;; TODO: don't hardcode tempo
             (Sequencer:setTempoInBPM sequencer 120)
             (Sequencer:start sequencer)))
          (loop))))))

(define (run!)
  (if (eq? initial-mode 'repl)
      (repl)
      (free-play)))

(define (quit!)
  ;; omitting this makes it hang...
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
       (Sequencer:setSequence sequencer stream)))))

(define (play-waka-file! path)
  (display "Playing Waka file... ")
  (flush-output-port)
  (let* ((input (path-data path))
         (sequence (parse input #t))
         (midi (sequence->midi sequence)))
    (play-midi!
     (lambda ()
       (Sequencer:setSequence sequencer midi)))))

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

;; TODO: allow changing octave in free play mode
;; TODO: change free play mode representation to match grammar (so
;; that you can copy-paste it for later usage)
;; TODO: read/save history file for repl mode
;; TODO: add subsequence syntax (like { ... })
;; TODO: add syntax for repeating notes/subsequences
;; TODO: check other alda syntax that's worth implementing (like
;; duration in ms/s)
;; TODO: add tempo customizable
;; TODO: implement sexps (instrument, tempo, sustain, ...)
