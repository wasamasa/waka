(define instrument-aliases (make-eq-hashtable))

(define (alias->instrument-id alias)
  (hashtable-ref instrument-aliases alias #f))

;; HACK

(hashtable-set! instrument-aliases 'percussion -1)

;; piano

(hashtable-set! instrument-aliases 'acoustic-grand-piano 0)
(hashtable-set! instrument-aliases 'grand-piano 0)
(hashtable-set! instrument-aliases 'piano 0)
(hashtable-set! instrument-aliases 'bright-acoustic-piano 1)
(hashtable-set! instrument-aliases 'bright-piano 1)
(hashtable-set! instrument-aliases 'electric-grand-piano 2)
(hashtable-set! instrument-aliases 'honky-tonk-piano 3)
(hashtable-set! instrument-aliases 'electric-piano-1 4)
(hashtable-set! instrument-aliases 'electric-piano 4)
(hashtable-set! instrument-aliases 'electric-piano-2 5)
(hashtable-set! instrument-aliases 'harpsichord 6)
(hashtable-set! instrument-aliases 'clavi 7)
(hashtable-set! instrument-aliases 'clavinet 7)

;; chromatic percussion

(hashtable-set! instrument-aliases 'celesta 8)
(hashtable-set! instrument-aliases 'celeste 8)
(hashtable-set! instrument-aliases 'glockenspiel 9)
(hashtable-set! instrument-aliases 'music-box 10)
(hashtable-set! instrument-aliases 'vibraphone 11)
(hashtable-set! instrument-aliases 'vibes 11)
(hashtable-set! instrument-aliases 'marimba 12)
(hashtable-set! instrument-aliases 'xylophone 13)
(hashtable-set! instrument-aliases 'tubular-bells 14)
(hashtable-set! instrument-aliases 'dulcimer 15)

;; organ

(hashtable-set! instrument-aliases 'drawbar-organ 16)
(hashtable-set! instrument-aliases 'percussive-organ 17)
(hashtable-set! instrument-aliases 'rock-organ 18)
(hashtable-set! instrument-aliases 'church-organ 19)
(hashtable-set! instrument-aliases 'reed-organ 20)
(hashtable-set! instrument-aliases 'accordion 21)
(hashtable-set! instrument-aliases 'harmonica 22)
(hashtable-set! instrument-aliases 'tango-accordion 23)

;; guitar

(hashtable-set! instrument-aliases 'acoustic-guitar-nylon 24)
(hashtable-set! instrument-aliases 'acoustic-guitar 24)
(hashtable-set! instrument-aliases 'guitar 24)
(hashtable-set! instrument-aliases 'acoustic-guitar-steel 25)
(hashtable-set! instrument-aliases 'electric-guitar-jazz 26)
(hashtable-set! instrument-aliases 'electric-guitar-clean 27)
(hashtable-set! instrument-aliases 'electric-guitar-palm-muted 28)
(hashtable-set! instrument-aliases 'electric-guitar-overdrive 29)
(hashtable-set! instrument-aliases 'electric-guitar-distorted 30)
(hashtable-set! instrument-aliases 'electric-guitar-harmonics 31)

;; bass

(hashtable-set! instrument-aliases 'acoustic-bass 32)
(hashtable-set! instrument-aliases 'upright-bass 32)
(hashtable-set! instrument-aliases 'electric-bass-finger 33)
(hashtable-set! instrument-aliases 'electric-bass 33)
(hashtable-set! instrument-aliases 'electric-bass-pick 34)
(hashtable-set! instrument-aliases 'fretless-bass 35)
(hashtable-set! instrument-aliases 'bass-slap 36)
(hashtable-set! instrument-aliases 'bass-pop 37)
(hashtable-set! instrument-aliases 'synth-bass-1 38)
(hashtable-set! instrument-aliases 'synth-bass 38)
(hashtable-set! instrument-aliases 'synth-bass-2 39)

;; strings

(hashtable-set! instrument-aliases 'violin 40)
(hashtable-set! instrument-aliases 'viola 41)
(hashtable-set! instrument-aliases 'cello 42)
(hashtable-set! instrument-aliases 'contrabass 43)
(hashtable-set! instrument-aliases 'string-bass 43)
(hashtable-set! instrument-aliases 'arco-bass 43)
(hashtable-set! instrument-aliases 'double-bass 43)
(hashtable-set! instrument-aliases 'tremolo-strings 44)
(hashtable-set! instrument-aliases 'pizzicato-strings 44)
(hashtable-set! instrument-aliases 'orchestral-harp 45)
(hashtable-set! instrument-aliases 'harp 46)
(hashtable-set! instrument-aliases 'timpani 47) ; this should be percussion...

;; ensemble

(hashtable-set! instrument-aliases 'string-ensemble-1 48)
(hashtable-set! instrument-aliases 'string-ensemble 48)
(hashtable-set! instrument-aliases 'string-ensemble-2 49)
(hashtable-set! instrument-aliases 'synth-strings-1 50)
(hashtable-set! instrument-aliases 'synth-strings 50)
(hashtable-set! instrument-aliases 'synth-strings-2 51)
(hashtable-set! instrument-aliases 'choir-aahs 52)
(hashtable-set! instrument-aliases 'voice-oohs 53)
(hashtable-set! instrument-aliases 'synth-voice 54)
(hashtable-set! instrument-aliases 'orchestra-hit 55)

;; brass

(hashtable-set! instrument-aliases 'trumpet 56)
(hashtable-set! instrument-aliases 'trombone 57)
(hashtable-set! instrument-aliases 'tuba 58)
(hashtable-set! instrument-aliases 'muted-trumpet 59)
(hashtable-set! instrument-aliases 'french-horn 60)
(hashtable-set! instrument-aliases 'brass-section 61)
(hashtable-set! instrument-aliases 'synth-brass-1 62)
(hashtable-set! instrument-aliases 'synth-brass 62)
(hashtable-set! instrument-aliases 'synth-brass-2 63)

;; reed

(hashtable-set! instrument-aliases 'soprano-saxophone 64)
(hashtable-set! instrument-aliases 'soprano-sax 64)
(hashtable-set! instrument-aliases 'alto-saxophone 65)
(hashtable-set! instrument-aliases 'alto-sax 65)
(hashtable-set! instrument-aliases 'tenor-saxophone 66)
(hashtable-set! instrument-aliases 'tenor-sax 66)
(hashtable-set! instrument-aliases 'baritone-saxophone 67)
(hashtable-set! instrument-aliases 'baritone-sax 67)
(hashtable-set! instrument-aliases 'bari-sax 67)
(hashtable-set! instrument-aliases 'oboe 68)
(hashtable-set! instrument-aliases 'english-horn 69)
(hashtable-set! instrument-aliases 'bassoon 70)
(hashtable-set! instrument-aliases 'clarinet 71)

;; pipe

(hashtable-set! instrument-aliases 'piccolo 72)
(hashtable-set! instrument-aliases 'flute 73)
(hashtable-set! instrument-aliases 'recorder 74)
(hashtable-set! instrument-aliases 'pan-flute 75)
(hashtable-set! instrument-aliases 'bottle 76)
(hashtable-set! instrument-aliases 'shakuhachi 77)
(hashtable-set! instrument-aliases 'whistle 78)
(hashtable-set! instrument-aliases 'ocarina 79)

;; synth lead

(hashtable-set! instrument-aliases 'square-lead 80)
(hashtable-set! instrument-aliases 'square-wave 80)
(hashtable-set! instrument-aliases 'square 80)
(hashtable-set! instrument-aliases 'saw-lead 81)
(hashtable-set! instrument-aliases 'saw-wave 81)
(hashtable-set! instrument-aliases 'saw 81)
(hashtable-set! instrument-aliases 'calliope-lead 82)
(hashtable-set! instrument-aliases 'calliope 82)
(hashtable-set! instrument-aliases 'chiffer-lead 83)
(hashtable-set! instrument-aliases 'chiffer 83)
(hashtable-set! instrument-aliases 'chiff 83)
(hashtable-set! instrument-aliases 'charang 84)
(hashtable-set! instrument-aliases 'solo-vox 85)
(hashtable-set! instrument-aliases 'fifths 86)
(hashtable-set! instrument-aliases 'sawtooth-fifths 86)
(hashtable-set! instrument-aliases 'bass-and-lead 87)
(hashtable-set! instrument-aliases 'bass+lead 87)

;; synth pad

(hashtable-set! instrument-aliases 'synth-pad-new-age 88)
(hashtable-set! instrument-aliases 'pad-new-age 88)
(hashtable-set! instrument-aliases 'new-age-pad 88)
(hashtable-set! instrument-aliases 'synth-pad-warm 89)
(hashtable-set! instrument-aliases 'pad-warm 89)
(hashtable-set! instrument-aliases 'warm-pad 89)
(hashtable-set! instrument-aliases 'synth-pad-polysynth 90)
(hashtable-set! instrument-aliases 'pad-polysynth 90)
(hashtable-set! instrument-aliases 'polysynth-pad 90)
(hashtable-set! instrument-aliases 'synth-pad-choir 91)
(hashtable-set! instrument-aliases 'pad-choir 91)
(hashtable-set! instrument-aliases 'choir-pad 91)
(hashtable-set! instrument-aliases 'synth-pad-bowed 92)
(hashtable-set! instrument-aliases 'pad-bowed 92)
(hashtable-set! instrument-aliases 'bowed-pad 92)
(hashtable-set! instrument-aliases 'pad-bowed-glass 92)
(hashtable-set! instrument-aliases 'bowed-glass-pad 92)
(hashtable-set! instrument-aliases 'synth-pad-metallic 93)
(hashtable-set! instrument-aliases 'pad-metallic 93)
(hashtable-set! instrument-aliases 'metallic-pad 93)
(hashtable-set! instrument-aliases 'pad-metal 93)
(hashtable-set! instrument-aliases 'metal-pad 93)
(hashtable-set! instrument-aliases 'synth-pad-halo 94)
(hashtable-set! instrument-aliases 'pad-halo 94)
(hashtable-set! instrument-aliases 'halo-pad 94)
(hashtable-set! instrument-aliases 'synth-pad-sweep 95)
(hashtable-set! instrument-aliases 'pad-sweep 95)
(hashtable-set! instrument-aliases 'sweep-pad 95)

;; synth effects

(hashtable-set! instrument-aliases 'fx-rain 96)
(hashtable-set! instrument-aliases 'fx-ice-rain 96)
(hashtable-set! instrument-aliases 'rain 96)
(hashtable-set! instrument-aliases 'ice-rain 96)
(hashtable-set! instrument-aliases 'fx-soundtrack 97)
(hashtable-set! instrument-aliases 'soundtrack 97)
(hashtable-set! instrument-aliases 'fx-crystal 98)
(hashtable-set! instrument-aliases 'crystal 98)
(hashtable-set! instrument-aliases 'fx-atmosphere 99)
(hashtable-set! instrument-aliases 'atmosphere 99)
(hashtable-set! instrument-aliases 'fx-brightness 100)
(hashtable-set! instrument-aliases 'brightness 100)
(hashtable-set! instrument-aliases 'fx-goblins 101)
(hashtable-set! instrument-aliases 'fx-goblin 101)
(hashtable-set! instrument-aliases 'goblins 101)
(hashtable-set! instrument-aliases 'goblin 101)
(hashtable-set! instrument-aliases 'fx-echoes 102)
(hashtable-set! instrument-aliases 'fx-echoe-drops 102)
(hashtable-set! instrument-aliases 'echoes 102)
(hashtable-set! instrument-aliases 'echoe-drops 102)
(hashtable-set! instrument-aliases 'fx-sci-fi 103)
(hashtable-set! instrument-aliases 'sci-fi 103)

;; ethnic

(hashtable-set! instrument-aliases 'sitar 104)
(hashtable-set! instrument-aliases 'banjo 105)
(hashtable-set! instrument-aliases 'shamisen 106)
(hashtable-set! instrument-aliases 'koto 107)
(hashtable-set! instrument-aliases 'kalimba 108)
(hashtable-set! instrument-aliases 'bagpipes 109)
(hashtable-set! instrument-aliases 'fiddle 110)
(hashtable-set! instrument-aliases 'shehnai 111)
(hashtable-set! instrument-aliases 'shahnai 111)
(hashtable-set! instrument-aliases 'shenai 111)
(hashtable-set! instrument-aliases 'shanai 111)

;; percussive

(hashtable-set! instrument-aliases 'tinkle-bell 112)
(hashtable-set! instrument-aliases 'tinker-bell 112)
(hashtable-set! instrument-aliases 'agogo 113)
(hashtable-set! instrument-aliases 'steel-drums 114)
(hashtable-set! instrument-aliases 'steel-drum 114)
(hashtable-set! instrument-aliases 'woodblock 115)
(hashtable-set! instrument-aliases 'taiko-drum 116)
(hashtable-set! instrument-aliases 'melodic-tom 117)
(hashtable-set! instrument-aliases 'synth-drum 118)
(hashtable-set! instrument-aliases 'reverse-cymbal 119)

;; sound effects

(hashtable-set! instrument-aliases 'guitar-fret-noise 120)
(hashtable-set! instrument-aliases 'breath-noise 121)
(hashtable-set! instrument-aliases 'seashore 122)
(hashtable-set! instrument-aliases 'bird-tweet 123)
(hashtable-set! instrument-aliases 'telephone-ring 124)
(hashtable-set! instrument-aliases 'helicopter 125)
(hashtable-set! instrument-aliases 'applause 126)
(hashtable-set! instrument-aliases 'gunshot 127)
(hashtable-set! instrument-aliases 'gun-shot 127)
