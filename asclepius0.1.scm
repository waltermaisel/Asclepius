#lang racket

# This is the software project that my bachelor's thesis was based on. It is a constraint-based 
# grammar for 1st species counterpoint itself based on the textbook on counterpoint written by 
# Arnold Schoenberg. Run as is, the program generates a bass and tenor voice cantus firmus and 
# counterpoint 8 notes in length in the key of C. Other voices are implemented, other keys aren't
# completely. I thought I would get through the entire book of course, but my advisor convinced me
# to stop here and develop my thesis argument in writing, which I did. That was, essentially, that
# Schoenberg, as demonstrated through his examples and verified by this software, did not very
# strictly apply his own rules. I talked about why that might be and what it might mean, attempted
# to connect it to other composers' practice and theory, and how that might relate to Schoenberg's
# own historical view.
#
# I basically intuited this backtracking algorithm and it could see some improvements. There is an
# obvious redundancy code that implements the algorithm that probably should be 
# abstracted/encapsulated. Also, the backtracking algorithm features no optimizations but is a 
# simple deapth-first search of the constraint space. It could use these improvments, not to 
# mention the other species and other keys and voices, canons, modulations, etc. much of the 
# infrastructure for these kinds of improvements has been designed in, some haven't been considered.
# 
# This program, named Asclepius, version 0.1, is written in Racket, a subdialect of Scheme, itself a
# dialect of LISP. For typesetting the music that it generates, it uses the Lilypond music 
# typesetting language. The lilypond file it produces should be ready to use with Lilypond to 
# produce a midi file and a .pdf file. Both of these programs are available for free on multiple  
# platforms. Lilypond 2.18.2 and Racket v6.7. 
#
# Copyright (c) 2016, Walter Maisel (waltermai@devio.us)
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. All advertising materials mentioning features or use of this software
#    must display the following acknowledgement:
#    This product includes software developed by Walter Maisel.
# 4. The name of Walter Maisel
#    may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY WALTER MAISEL ''AS IS'' AND ANY
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL WALTER MAISEL BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;currently for circular-list
(require srfi/1)

;makes a list of sequential (+1) numbers from x to y, inclusive
(define (series x y)
  (cond ((not (list? x)) (series (list x) y))
        ((= (last x) y) x)
        (else (series (append x (list (+ (last x) 1))) y))))

;definitions of ambiti per clef (may have to alter when doublesharps/flats are introduced)
(define treble-ambitus
  (series 61 80))
(define soprano-ambitus
  (series 58 77))
(define mezzosoprano-ambitus
  (series 54 73))
(define alto-ambitus
  (series 51 70))
(define tenor-ambitus
  (series 48 67))
(define baritone-ambitus
  (series 44 63))
(define bass-ambitus
  (series 41 59))

;;definitions for diatonic key system (just major keys for now)
(define major-key-intervals
  (circular-list cadr cadr car cadr cadr cadr car))
(define major-key-remainders
  (circular-list cddr cddr cdr cddr cddr cddr cdr))

;
;(define note-names (make-vector 127)) (for-each-instance-of-a-horrible-elaborate-pattern-of-note-names (lambda (name number) (vector-set! note-names number ;name))) (define (number->note-name number) (vector-ref note-names number))

;converts midinums to lilypond notenames, previous commented out bit of code may help to optimize, can be somehow compressed by series and pattern
(define (midi-to-ly sharp-or-flat x)
 (cond ((equal? sharp-or-flat 'flat)
        (cond ((= x 0) "c,,,,")
              ((= x 1) "des,,,,")
              ((= x 2) "d,,,,")
              ((= x 3) "ees,,,,")
              ((= x 4) "e,,,,")
              ((= x 5) "f,,,,")
              ((= x 6) "ges,,,,")
              ((= x 7) "g,,,,")
              ((= x 8) "aes,,,,")
              ((= x 9) "a,,,,")
              ((= x 10) "bes,,,,")
              ((= x 11) "b,,,,")
              ((= x 12) "c,,,")
              ((= x 13) "des,,,")
              ((= x 14) "d,,,")
              ((= x 15) "ees,,,")
              ((= x 16) "e,,,")
              ((= x 17) "f,,,")
              ((= x 18) "ges,,,")
              ((= x 19) "g,,,")
              ((= x 20) "aes,,,")
              ((= x 21) "a,,,")
              ((= x 22) "bes,,,")
              ((= x 23) "b,,,")
              ((= x 24) "c,,")
              ((= x 25) "des,,")
              ((= x 26) "d,,")
              ((= x 27) "ees,,")
              ((= x 28) "e,,")
              ((= x 29) "f,,")
              ((= x 30) "ges,,")
              ((= x 31) "g,,")
              ((= x 32) "aes,,")
              ((= x 33) "a,,")
              ((= x 34) "bes,,")
              ((= x 35) "b,,")
              ((= x 36) "c,")
              ((= x 37) "des,")
              ((= x 38) "d,")
              ((= x 39) "ees,")
              ((= x 40) "e,")
              ((= x 41) "f,")
              ((= x 42) "ges,")
              ((= x 43) "g,")
              ((= x 44) "aes,")
              ((= x 45) "a,")
              ((= x 46) "bes,")
              ((= x 47) "b,")
              ((= x 48) "c")
              ((= x 49) "des")
              ((= x 50) "d")
              ((= x 51) "ees")
              ((= x 52) "e")
              ((= x 53) "f")
              ((= x 54) "ges")
              ((= x 55) "g")
              ((= x 56) "aes")
              ((= x 57) "a")
              ((= x 58) "bes")
              ((= x 59) "b")
              ((= x 60) "c'")
              ((= x 61) "des'")
              ((= x 62) "d'")
              ((= x 63) "ees'")
              ((= x 64) "e'")
              ((= x 65) "f'")
              ((= x 66) "ges'")
              ((= x 67) "g'")
              ((= x 68) "aes'")
              ((= x 69) "a'")
              ((= x 70) "bes'")
              ((= x 71) "b'")
              ((= x 72) "c''")
              ((= x 73) "des''")
              ((= x 74) "d''")
              ((= x 75) "ees''")
              ((= x 76) "e''")
              ((= x 77) "f''")
              ((= x 78) "ges''")
              ((= x 79) "g''")
              ((= x 80) "aes''")
              ((= x 81) "a''")
              ((= x 82) "bes''")
              ((= x 83) "b''")
              ((= x 84) "c'''")
              ((= x 85) "des'''")
              ((= x 86) "d'''")
              ((= x 87) "ees'''")
              ((= x 88) "e'''")
              ((= x 89) "f'''")
              ((= x 90) "ges'''")
              ((= x 91) "g'''")
              ((= x 92) "aes'''")
              ((= x 93) "a'''")
              ((= x 94) "bes'''")
              ((= x 95) "b'''")
              ((= x 96) "c''''")
              ((= x 97) "des''''")
              ((= x 98) "d''''")
              ((= x 99) "ees''''")
              ((= x 100) "e''''")
              ((= x 101) "f''''")
              ((= x 102) "ges''''")
              ((= x 103) "g''''")
              ((= x 104) "aes''''")
              ((= x 105) "a''''")
              ((= x 106) "bes''''")
              ((= x 107) "b''''")
              ((= x 108) "c'''''")
              ((= x 109) "des'''''")
              ((= x 110) "d'''''")
              ((= x 111) "ees'''''")
              ((= x 112) "e'''''")
              ((= x 113) "f'''''")
              ((= x 114) "ges'''''")
              ((= x 115) "g'''''")
              ((= x 116) "aes'''''")
              ((= x 117) "a'''''")
              ((= x 118) "bes'''''")
              ((= x 119) "b'''''")
              ((= x 120) "c''''''")
              ((= x 121) "des''''''")
              ((= x 122) "d''''''")
              ((= x 123) "ees''''''")
              ((= x 124) "e''''''")
              ((= x 125) "f''''''")
              ((= x 126) "ges''''''")
              ((= x 127) "g''''''")))
       ((equal? sharp-or-flat 'sharp)
        (cond ((= x 0) "c,,,,")
              ((= x 1) "cis,,,,")
              ((= x 2) "d,,,,")
              ((= x 3) "dis,,,,")
              ((= x 4) "e,,,,")
              ((= x 5) "f,,,,")
              ((= x 6) "fis,,,,")
              ((= x 7) "g,,,,")
              ((= x 8) "gis,,,,")
              ((= x 9) "a,,,,")
              ((= x 10) "ais,,,,")
              ((= x 11) "b,,,,")
              ((= x 12) "c,,,")
              ((= x 13) "cis,,,")
              ((= x 14) "d,,,")
              ((= x 15) "dis,,,")
              ((= x 16) "e,,,")
              ((= x 17) "f,,,")
              ((= x 18) "fis,,,")
              ((= x 19) "g,,,")
              ((= x 20) "gis,,,")
              ((= x 21) "a,,,")
              ((= x 22) "ais,,,")
              ((= x 23) "b,,,")
              ((= x 24) "c,,")
              ((= x 25) "cis,,")
              ((= x 26) "d,,")
              ((= x 27) "dis,,")
              ((= x 28) "e,,")
              ((= x 29) "f,,")
              ((= x 30) "fis,,")
              ((= x 31) "g,,")
              ((= x 32) "gis,,")
              ((= x 33) "a,,")
              ((= x 34) "ais,,")
              ((= x 35) "b,,")
              ((= x 36) "c,")
              ((= x 37) "cis,")
              ((= x 38) "d,")
              ((= x 39) "dis,")
              ((= x 40) "e,")
              ((= x 41) "f,")
              ((= x 42) "fis,")
              ((= x 43) "g,")
              ((= x 44) "gis,")
              ((= x 45) "a,")
              ((= x 46) "ais,")
              ((= x 47) "b,")
              ((= x 48) "c")
              ((= x 49) "cis")
              ((= x 50) "d")
              ((= x 51) "dis")
              ((= x 52) "e")
              ((= x 53) "f")
              ((= x 54) "fis")
              ((= x 55) "g")
              ((= x 56) "gis")
              ((= x 57) "a")
              ((= x 58) "ais")
              ((= x 59) "b")
              ((= x 60) "c'")
              ((= x 61) "cis'")
              ((= x 62) "d'")
              ((= x 63) "dis'")
              ((= x 64) "e'")
              ((= x 65) "f'")
              ((= x 66) "fis'")
              ((= x 67) "g'")
              ((= x 68) "gis'")
              ((= x 69) "a'")
              ((= x 70) "ais'")
              ((= x 71) "b'")
              ((= x 72) "c''")
              ((= x 73) "cis''")
              ((= x 74) "d''")
              ((= x 75) "dis''")
              ((= x 76) "e''")
              ((= x 77) "f''")
              ((= x 78) "fis''")
              ((= x 79) "g''")
              ((= x 80) "gis''")
              ((= x 81) "a''")
              ((= x 82) "ais''")
              ((= x 83) "b''")
              ((= x 84) "c'''")
              ((= x 85) "cis'''")
              ((= x 86) "d'''")
              ((= x 87) "dis'''")
              ((= x 88) "e'''")
              ((= x 89) "f'''")
              ((= x 90) "fis'''")
              ((= x 91) "g'''")
              ((= x 92) "gis'''")
              ((= x 93) "a'''")
              ((= x 94) "ais'''")
              ((= x 95) "b'''")
              ((= x 96) "c''''")
              ((= x 97) "cis''''")
              ((= x 98) "d''''")
              ((= x 99) "dis''''")
              ((= x 100) "e''''")
              ((= x 101) "f''''")
              ((= x 102) "fis''''")
              ((= x 103) "g''''")
              ((= x 104) "gis''''")
              ((= x 105) "a''''")
              ((= x 106) "ais''''")
              ((= x 107) "b''''")
              ((= x 108) "c'''''")
              ((= x 109) "cis'''''")
              ((= x 110) "d'''''")
              ((= x 111) "dis'''''")
              ((= x 112) "e'''''")
              ((= x 113) "f'''''")
              ((= x 114) "fis'''''")
              ((= x 115) "g'''''")
              ((= x 116) "gis'''''")
              ((= x 117) "a'''''")
              ((= x 118) "ais'''''")
              ((= x 119) "b'''''")
              ((= x 120) "c''''''")
              ((= x 121) "cis''''''")
              ((= x 122) "d''''''")
              ((= x 123) "dis''''''")
              ((= x 124) "e''''''")
              ((= x 125) "f''''''")
              ((= x 126) "fis''''''")
              ((= x 127) "g''''''")))))

;;procedure that yields value of tonic from bottom of ambitus for a given clef and pitch-class
(define (tonic clef-ambitus tonic-class)
  (if (= (remainder (- (car clef-ambitus) tonic-class)
                    12)
         0)
      (car clef-ambitus)
      (tonic (cdr clef-ambitus) tonic-class)))

;; the problem now is that this iterates by half-step instead of using the proper intervals! (old comment)
(define (make-degrees initial-degree initial-midi-number rest-of-ambitus list-of-degrees interval-list remainder-list step)
  (cond ((equal? initial-degree 'stop)
         list-of-degrees)
        ((null? rest-of-ambitus)
         (make-degrees 'stop 
                       rest-of-ambitus 
                       rest-of-ambitus 
                       (append list-of-degrees 
                               (list (cons initial-degree initial-midi-number)))
                       interval-list
                       remainder-list
                       step))
        ((null? list-of-degrees)
         (make-degrees (+ 1 initial-degree) 
                       ((list-ref interval-list step) rest-of-ambitus)  
                       ((list-ref remainder-list step) rest-of-ambitus) 
                       (list (cons initial-degree initial-midi-number))
                       interval-list
                       remainder-list
                       (+ 1 step)))  
        ((and (and (not (= (remainder step 7) 
                           2)) 
                   (not (= (remainder step 7) 
                           6)) 
                   (not (= step 2)) 
                   (not (= step 6)))
              (= (length rest-of-ambitus) 1))
         (make-degrees 'stop 
                       rest-of-ambitus 
                       rest-of-ambitus 
                       (append list-of-degrees 
                               (list (cons initial-degree initial-midi-number)))
                       interval-list
                       remainder-list
                       step))
        (else (make-degrees (+ 1 initial-degree) 
                            ((list-ref interval-list step) rest-of-ambitus) 
                            ((list-ref remainder-list step) rest-of-ambitus) 
                            (append list-of-degrees 
                                    (list (cons initial-degree initial-midi-number)))
                            interval-list
                            remainder-list
                            (+ 1 step)))))

;; procedure that finds bottom diatonic degree below tonic and initiates the process of assigning degree numbers in ambitus up from that (only major!)
(define (find-bottom-degree-iter clef-ambitus tonic-class degree current-tonic)
  (cond ((= (- current-tonic 10) 
            (car clef-ambitus)) 
         (make-degrees -5 (car clef-ambitus) (cdr clef-ambitus) '() major-key-intervals major-key-remainders 1)) 
        ((= (- current-tonic 8) 
            (car clef-ambitus)) 
         (make-degrees -4 (car clef-ambitus) (cdr clef-ambitus) '() major-key-intervals major-key-remainders 2))
        ((= (- current-tonic 7) 
            (car clef-ambitus)) 
         (make-degrees -3 (car clef-ambitus) (cdr clef-ambitus) '() major-key-intervals major-key-remainders 3))
        ((= (- current-tonic 5) 
            (car clef-ambitus)) 
         (make-degrees -2 (car clef-ambitus) (cdr clef-ambitus) '() major-key-intervals major-key-remainders 4))
        ((= (- current-tonic 3) 
            (car clef-ambitus)) 
         (make-degrees -1 (car clef-ambitus) (cdr clef-ambitus) '() major-key-intervals major-key-remainders 5))
        ((= (- current-tonic 1) 
            (car clef-ambitus)) 
         (make-degrees 0 (car clef-ambitus) (cdr clef-ambitus) '() major-key-intervals major-key-remainders 6))
        ((= current-tonic 
            (car clef-ambitus)) 
         (make-degrees 1 (car clef-ambitus) (cdr clef-ambitus) '() major-key-intervals major-key-remainders 0))
        (else (find-bottom-degree-iter (cdr clef-ambitus) tonic-class degree current-tonic))))

(define (assign-key clef-ambitus tonic-class) 
  (define current-tonic (tonic clef-ambitus tonic-class))
  (find-bottom-degree-iter clef-ambitus tonic-class -5 current-tonic))

; checks for adjacent or compound tritones
;; could perhaps only check latest notes instead of recurring so much
(define (tritoneless? checked-notes)
  (cond ((= (length checked-notes) 1)
         #t)
        ((= (length checked-notes) 2)
         (cond ((or (and (= (modulo (car (first checked-notes)) 7) 4)     
                         (= (modulo (car (second checked-notes)) 7) 0))
                    (and (= (modulo (car (first checked-notes)) 7) 0) 
                         (= (modulo (car (second checked-notes)) 7) 4)))
                #f)
               (else (tritoneless? (list-tail checked-notes 1)))))
        ((= (length checked-notes) 3)
         (cond ((or (and (= (modulo (car (first checked-notes)) 7) 4)     
                         (= (modulo (car (second checked-notes)) 7) 0))
                    (and (= (modulo (car (first checked-notes)) 7) 0) 
                         (= (modulo (car (second checked-notes)) 7) 4))
                    (and (= (modulo (car (first checked-notes)) 7) 4)     
                         (= (modulo (car (third checked-notes)) 7) 0))
                    (and (= (modulo (car (first checked-notes)) 7) 0) 
                         (= (modulo (car (third checked-notes)) 7) 4)))
                #f)
               (else (tritoneless? (list-tail checked-notes 1)))))
        ((= (length checked-notes) 4)
         (cond ((or (and (= (modulo (car (first checked-notes)) 7) 4)     
                         (= (modulo (car (second checked-notes)) 7) 0))
                    (and (= (modulo (car (first checked-notes)) 7) 0) 
                         (= (modulo (car (second checked-notes)) 7) 4))
                    (and (= (modulo (car (first checked-notes)) 7) 4)     
                         (= (modulo (car (third checked-notes)) 7) 0))
                    (and (= (modulo (car (first checked-notes)) 7) 0) 
                         (= (modulo (car (third checked-notes)) 7) 4))
                    (and (= (modulo (car (first checked-notes)) 7) 4)     
                         (= (modulo (car (fourth checked-notes)) 7) 0))
                    (and (= (modulo (car (first checked-notes)) 7) 0) 
                         (= (modulo (car (fourth checked-notes)) 7) 4)))
                #f)
               (else (tritoneless? (list-tail checked-notes 1)))))
        ((or (and (= (modulo (car (first checked-notes)) 7) 4)     
                  (= (modulo (car (second checked-notes)) 7) 0))
             (and (= (modulo (car (first checked-notes)) 7) 0) 
                  (= (modulo (car (second checked-notes)) 7) 4))
             (and (= (modulo (car (first checked-notes)) 7) 4)     
                  (= (modulo (car (third checked-notes)) 7) 0))
             (and (= (modulo (car (first checked-notes)) 7) 0) 
                  (= (modulo (car (third checked-notes)) 7) 4)) 
             (and (= (modulo (car (first checked-notes)) 7) 4)     
                  (= (modulo (car (fourth checked-notes)) 7) 0))
             (and (= (modulo (car (first checked-notes)) 7) 0) 
                  (= (modulo (car (fourth checked-notes)) 7) 4))
             (and (= (modulo (car (first checked-notes)) 7) 4)     
                  (= (modulo (car (fifth checked-notes)) 7) 0))
             (and (= (modulo (car (first checked-notes)) 7) 0) 
                  (= (modulo (car (fifth checked-notes)) 7) 4)))
         #f)
        (else (tritoneless? (list-tail checked-notes 1)))))

; No leaps of 7th, 9th, or 11th or greater
(define (nobigbadleaps? checked-notes)
  (cond ((= (length checked-notes) 1) #t)
        ((= (length checked-notes) 2)
         (cond ((= (abs (- (car (first checked-notes)) (car (second checked-notes)))) 6) #f)
               ((= (abs (- (car (first checked-notes)) (car (second checked-notes)))) 8) #f)
               ((>= (abs (- (car (first checked-notes)) (car (second checked-notes)))) 9) #f)
               (else (nobigbadleaps? (list-tail checked-notes 1)))))
        ((= (abs (- (car (first checked-notes)) (car (second checked-notes)))) 6) #f)
        ((= (abs (- (car (first checked-notes)) (car (second checked-notes)))) 8) #f)
        ((>= (abs (- (car (first checked-notes)) (car (second checked-notes)))) 9) #f)
        ((= (abs (- (car (first checked-notes)) (car (third checked-notes)))) 6) #f)
        ((= (abs (- (car (first checked-notes)) (car (third checked-notes)))) 8) #f)
        ((>= (abs (- (car (first checked-notes)) (car (third checked-notes)))) 9) #f)
        (else (nobigbadleaps? (list-tail checked-notes 1)))))
  
;checks if notes continue for more than 9 tones in 1 direction
(define (samedirtoolong? checked-notes)
  (define (samedir-iter checked-notes dir howlong)
    (cond ((= (length checked-notes) 1) #f)
          ((= howlong 10) #t)
          ((= (- (car (first checked-notes)) (car (second checked-notes))) 0)
           (samedir-iter (list-tail checked-notes 1) dir (+ howlong 1)))
          ((and (> (- (car (first checked-notes)) (car (second checked-notes))) -1)
                (or (eq? dir 'down) (eq? dir 'neutral)))
           (samedir-iter (list-tail checked-notes 1) 'down  (+ howlong 1)))
          ((and (> (- (car (first checked-notes)) (car (second checked-notes))) -1)
                (or (eq? dir 'up) (eq? dir 'neutral)))
           (samedir-iter (list-tail checked-notes 1) 'down  1))
          ((and (< (- (car (first checked-notes)) (car (second checked-notes))) 1)
                (or (eq? dir 'up) (eq? dir 'neutral)))
           (samedir-iter (list-tail checked-notes 1) 'up (+ howlong 1)))
          ((and (< (- (car (first checked-notes)) (car (second checked-notes))) 1)
                (or (eq? dir 'down) (eq? dir 'neutral)))
           (samedir-iter (list-tail checked-notes 1) 'up 0))))
  (samedir-iter checked-notes 'neutral 1))

;checks if leaps are balanced by motion in opposite direction
(define (balancedleaps? checked-notes)
  (cond ((= (length checked-notes) 1) #t)
        ((= (length checked-notes) 2) #t)
        ((= (length checked-notes) 3)
         (cond ((and (> (- (car (first checked-notes)) (car (second checked-notes))) 2)
                     (not (< (- (car (second checked-notes)) (car (third checked-notes))) 0)))
                #f)
               ((and (< (- (car (first checked-notes)) (car (second checked-notes))) -2)
                     (not (> (- (car (second checked-notes)) (car (third checked-notes))) 0)))
                #f)
               (else #t)))
        ((or (and (= (- (car (first checked-notes)) (car (third checked-notes))) 5)
                  (> (car (first checked-notes)) (car (second checked-notes)))
                  (> (car (second checked-notes)) (car (third checked-notes)))
                  (not (= (- (car (first checked-notes)) (car (second checked-notes))) 4)))            
             (and (= (- (car (first checked-notes)) (car (third checked-notes))) 7)
                  (> (car (first checked-notes)) (car (second checked-notes)))
                  (> (car (second checked-notes)) (car (third checked-notes))))
             (and (= (- (car (first checked-notes)) (car (third checked-notes))) -5)
                  (< (car (first checked-notes)) (car (second checked-notes)))
                  (< (car (second checked-notes)) (car (third checked-notes)))
                  (not (= (- (car (first checked-notes)) (car (second checked-notes))) -4)))
             (and (= (- (car (first checked-notes)) (car (third checked-notes))) -7)
                  (< (car (first checked-notes)) (car (second checked-notes)))
                  (< (car (second checked-notes)) (car (third checked-notes)))))
         (balancedleaps? (list-tail checked-notes 1)))         
        ((and (> (- (car (first checked-notes)) (car (second checked-notes))) 2)
              (not (< (- (car (second checked-notes)) (car (third checked-notes))) 0)))
         #f)
        ((and (< (- (car (first checked-notes)) (car (second checked-notes))) -2)
              (not (> (- (car (second checked-notes)) (car (third checked-notes))) 0)))
         #f)
        (else (balancedleaps? (list-tail checked-notes 1)))))

; checks for leaps of a sixth
(define (nosixthleaps? checked-notes)
  (cond ((= (length checked-notes) 1) #t)
        ((= (abs (- (first (map car checked-notes)) (second (map car checked-notes)))) 5) #f)
        (else (nosixthleaps? (list-tail checked-notes 1)))))

; checks for leaps of an octave
(define (nooctaveleaps? checked-notes)
  (cond ((= (length checked-notes) 1) #t)
        ((= (abs (- (first (map car checked-notes)) (second (map car checked-notes)))) 7) #f)
        (else (nooctaveleaps? (list-tail checked-notes 1)))))

; checks for triads and seventh chords in any *four* adjacent tones
(define (noadjtriadsor7thchords? checked-notes)
  (let ((transposed-chord (cond ((>= (length checked-notes) 4)
                                 (map - (delete-duplicates (sort (map (lambda (x) (modulo x 7)) (list (car (first checked-notes)) 
                                                                                                      (car (second checked-notes)) 
                                                                                                      (car (third checked-notes)) 
                                                                                                      (car (fourth checked-notes))))
                                                                 <))
                                      (make-list 4 (- (first (delete-duplicates (sort (map (lambda (x) (modulo x 7)) (list (car (first checked-notes)) 
                                                                                                                           (car (second checked-notes)) 
                                                                                                                           (car (third checked-notes)) 
                                                                                                                           (car (fourth checked-notes))))
                                                                                      <))) 1)))))))
    (cond ((<= (length checked-notes) 3) #t)
          ((and (>= (length checked-notes) 4)
                (or (equal? transposed-chord
                            '(1 3 5))
                    (equal? transposed-chord
                            '(1 3 6))
                    (equal? transposed-chord
                            '(1 4 6))
                    (equal? transposed-chord
                            '(0 1 3 5))))
           #f)
          (else (noadjtriadsor7thchords? (list-tail checked-notes 1))))))

; checks to see if any note is repeated within the span of 9 notes
(define (nonoterepetition? checked-notes)
  (cond ((< (length checked-notes) 3) #t)
        ((> (length checked-notes) 8)
         (cond ((> (count (lambda (x) (equal? x (car (first checked-notes)))) 
                          (take (map car checked-notes) 8)) 
                   2) 
                #f)
               (else (nonoterepetition? (list-tail checked-notes 1)))))
        ((> (count (lambda (x) (equal? x (car (first checked-notes))))
                   (map car checked-notes))
            2)
         #f)
        (else (nonoterepetition? (list-tail checked-notes 1)))))

; checks if range of notes is wide enough in given length
(define (rangewideenough? checked-notes)
  (cond ((< (length checked-notes) 7) #t)
        ((< (length checked-notes) 9)
         (cond ((< (- (apply max (take (map car checked-notes) 7)) (apply min (take (map car checked-notes) 7))) 4) #f)
               (else (rangewideenough? (list-tail checked-notes 1)))))
        ((>= (length checked-notes) 9)
         (cond ((or (< (- (apply max (take (map car checked-notes) 9)) (apply min (take (map car checked-notes) 9))) 5)
                    (< (- (apply max (take (map car checked-notes) 7)) (apply min (take (map car checked-notes) 7))) 4)) 
                #f)
               (else (rangewideenough? (list-tail checked-notes 1)))))))

; checks if there are any sequences
(define (nosequences? checked-notes)
  (define (noseq-iter checked-notes seq-length)
    (cond ((<= (length checked-notes) 3) #t)
          ((<= seq-length 2) (noseq-iter (list-tail checked-notes 1) (quotient (- (length checked-notes) 1) 2)))
          ((equal? (map - 
                        (take (map car checked-notes) seq-length) 
                        (make-list seq-length (- (first (map car checked-notes)) 1)))
                   (map -
                        (take (list-tail (map car checked-notes) seq-length) seq-length)
                        (make-list seq-length (- (list-ref (map car checked-notes) seq-length) 1)))) #f)
          (else (noseq-iter checked-notes (- seq-length 1)))))
  (noseq-iter checked-notes (quotient (length checked-notes) 2)))

;checks to see if the first tone is acceptable
(define (first? checked-notes voice-list cn-ambitus tonic-class)
  (cond ((and (= (first voice-list) (last (sort voice-list <)))
              (= (car (first checked-notes)) 1)) #t)
        ((and (= (first voice-list) (first (sort voice-list <)))
              (ormap (lambda (x) (= x (- (caar (degrees-to-midinums '((1 . 1)) cn-ambitus tonic-class)) (first cn-ambitus)))) (series 5 10))
              (= (car (first checked-notes)) -2)) #f)
        ((and (not (= (first voice-list) (last (sort voice-list <))))
              (or (= (modulo (car (first checked-notes)) 7) 1)
                  (= (modulo (car (first checked-notes)) 7) 3)
                  (= (modulo (car (first checked-notes)) 7) 5))) #t)
        (else #f)))

;checks to see if there are more than 2 leaps in a row
(define (no3consecutiveleaps? checked-notes)
  (cond ((< (length checked-notes) 4) #t)
        ((and (> (abs (- (car (first checked-notes)) (car (second checked-notes)))) 1)
              (> (abs (- (car (second checked-notes)) (car (third checked-notes)))) 1)
              (> (abs (- (car (third checked-notes)) (car (fourth checked-notes)))) 1)) #f)
        (else (no3consecutiveleaps? (rest checked-notes)))))
              
; checks to see if the penultimate tone is acceptable
(define (penultimate? checked-notes number-of-measures voice-list)
  (cond ((< (length checked-notes) (- number-of-measures 1)) #t)
        ((and (= (first voice-list) (last (sort voice-list <)))
              (or (= (modulo (list-ref (map car checked-notes) (- number-of-measures 2)) 7) 5)
                  (= (modulo (list-ref (map car checked-notes) (- number-of-measures 2)) 7) 0)
                  (= (modulo (list-ref (map car checked-notes) (- number-of-measures 2)) 7) 2))) #t)
        ((and (not (= (first voice-list) (last (sort voice-list <))))
              (or (= (modulo (list-ref (map car checked-notes) (- number-of-measures 2)) 7) 5)
                  (= (modulo (list-ref (map car checked-notes) (- number-of-measures 2)) 7) 0)
                  (= (modulo (list-ref (map car checked-notes) (- number-of-measures 2)) 7) 2)
                  (= (modulo (list-ref (map car checked-notes) (- number-of-measures 2)) 7) 4))) #t)
        (else #f)))

; checks to see if the ultimate tone is acceptable
(define (ultimate? checked-notes number-of-measures voice-list)
  (cond ((< (length checked-notes) number-of-measures) #t)
        ((and (= (first voice-list) (last (sort voice-list <)))
              (= (modulo (list-ref (map car checked-notes) (- number-of-measures 1)) 7) 1)) #t)
        ((and (not (= (first voice-list) (last (sort voice-list <))))
              (or (= (modulo (list-ref (map car checked-notes) (- number-of-measures 1)) 7) 5)
                  (= (modulo (list-ref (map car checked-notes) (- number-of-measures 1)) 7) 3)
                  (= (modulo (list-ref (map car checked-notes) (- number-of-measures 1)) 7) 1))) #t)
        (else #f)))

;meta-predicate including all cf predicates
(define (allcfconstraints? checked-notes number-of-measures voice-list cn-ambitus tonic-class)
  (cond ((and (first? checked-notes voice-list cn-ambitus tonic-class)
              (nobigbadleaps? checked-notes) 
              (tritoneless? checked-notes)
              (not (samedirtoolong? checked-notes))
              (balancedleaps? checked-notes)
              (nosixthleaps? checked-notes)
              (nooctaveleaps? checked-notes)
              (noadjtriadsor7thchords? checked-notes)
              (nonoterepetition? checked-notes)
              (rangewideenough? checked-notes)
              (nosequences? checked-notes)
              (no3consecutiveleaps? checked-notes)
              (penultimate? checked-notes number-of-measures voice-list)
              (ultimate? checked-notes number-of-measures voice-list)) #t)
        (else #f)))

;meta-predicate including all cf predicates except 8ve and 6th leaps for 2nd tier options
(define (2ndtierconstraints? checked-notes number-of-measures voice-list cn-ambitus tonic-class)
  (cond ((and (first? checked-notes voice-list cn-ambitus tonic-class)
              (nobigbadleaps? checked-notes) 
              (tritoneless? checked-notes)
              (not (samedirtoolong? checked-notes))
              (balancedleaps? checked-notes)
              (noadjtriadsor7thchords? checked-notes)
              (nonoterepetition? checked-notes)
              (rangewideenough? checked-notes)
              (nosequences? checked-notes)
              (no3consecutiveleaps? checked-notes)
              (penultimate? checked-notes number-of-measures voice-list)
              (ultimate? checked-notes number-of-measures voice-list)) #t)
        (else #f)))

;makes list of all notes given a cf-list that are well formed as next degree
(define (wellformeddegrees cf-viable-notes cf-list number-of-measures voice-list cn-ambitus tonic-class)
  (define (wellformeddegrees-iter cf-viable-notes wellformedlist)
    (cond ((null? cf-viable-notes) wellformedlist)
          ((allcfconstraints? (append cf-list (list (cons (car (first cf-viable-notes)) 
                                                          1))) 
                              number-of-measures 
                              voice-list
                              cn-ambitus
                              tonic-class)
           (wellformeddegrees-iter (list-tail cf-viable-notes 1) (append wellformedlist (list (car (first cf-viable-notes))))))
          (else (wellformeddegrees-iter (list-tail cf-viable-notes 1) wellformedlist))))
  (wellformeddegrees-iter cf-viable-notes '()))
                                 
;makes list of all notes given a cf-list that are well formed as next degree, including "emergency", 2nd tier notes
(define (2ndtierdegrees cf-viable-notes cf-list number-of-measures voice-list cn-ambitus tonic-class)
  (define (2ndtierdegrees-iter cf-viable-notes wellformedlist)
    (cond ((null? cf-viable-notes) wellformedlist)
          ((2ndtierconstraints? (append cf-list (list (cons (car (first cf-viable-notes)) 
                                                            1))) 
                                number-of-measures 
                                voice-list
                                cn-ambitus
                                tonic-class)
           (2ndtierdegrees-iter (list-tail cf-viable-notes 1) (append wellformedlist (list (car (first cf-viable-notes))))))
          (else (2ndtierdegrees-iter (list-tail cf-viable-notes 1) wellformedlist))))
  (2ndtierdegrees-iter cf-viable-notes '()))  

;procedure that writes the cantus firmus
(define (make-cf clef-ambitus tonic-class number-of-measures voice-list)
  (let ((cf-viable-notes (assign-key clef-ambitus tonic-class)))
    (define (make-cf-iter duration cf-list bad-degrees)
      (cond ((and (= (length cf-list) number-of-measures)
                  (2ndtierconstraints? cf-list number-of-measures voice-list clef-ambitus tonic-class))
             cf-list)
            ((> (length (remove (lambda (x) (member x (cond ((assoc (+ (length cf-list) 
                                                                       1) 
                                                                    bad-degrees) => 
                                                                                 (lambda (x) (cdr x)))
                                                            (else '()))))
                                (wellformeddegrees cf-viable-notes 
                                                   cf-list 
                                                   number-of-measures 
                                                   voice-list
                                                   clef-ambitus
                                                   tonic-class)))
                0)
	     (display (+ 1 (length cf-list)))
             (display " bad-degrees: ")
             (display bad-degrees)
             (display " cf-list: ")
             (display cf-list)
             (newline)
             (make-cf-iter 1
                           (append cf-list 
                                   (list (cons (list-ref (remove (lambda (x) (member x (cond ((assoc (+ (length cf-list) 
                                                                                                        1) 
                                                                                                     bad-degrees) => 
                                                                                                     (lambda (x) (cdr x)))
                                                                                             (else '()))))
                                                                 (wellformeddegrees cf-viable-notes 
                                                                                    cf-list 
                                                                                    number-of-measures 
                                                                                    voice-list
                                                                                    clef-ambitus
                                                                                    tonic-class))
                                                         (random (length (remove (lambda (x) (member x (cond ((assoc (+ (length cf-list) 1) 
                                                                                                                     bad-degrees) => 
                                                                                                                     (lambda (x) (cdr x)))
                                                                                                             (else '()))))
                                                                                 (wellformeddegrees cf-viable-notes 
                                                                                                    cf-list 
                                                                                                    number-of-measures 
                                                                                                    voice-list
                                                                                                    clef-ambitus
                                                                                                    tonic-class)))))
                                               duration)))
                           bad-degrees))
            ((> (length (remove (lambda (x) (member x (cond ((assoc (+ (length cf-list) 
                                                                       1) 
                                                                    bad-degrees) => 
                                                                                 (lambda (x) (cdr x)))
                                                            (else '()))))
                                (2ndtierdegrees cf-viable-notes 
                                                cf-list 
                                                number-of-measures 
                                                voice-list
                                                clef-ambitus
                                                tonic-class)))
                0)
	     (display (+ 1 (length cf-list)))
             (display " bad-degrees: ")
             (display bad-degrees)
             (display " cf-list: ")
             (display cf-list)
             (newline)
             (make-cf-iter 1
                           (append cf-list 
                                   (list (cons (list-ref (remove (lambda (x) (member x (cond ((assoc (+ (length cf-list) 
                                                                                                        1) 
                                                                                                     bad-degrees) => 
                                                                                                     (lambda (x) (cdr x)))
                                                                                             (else '()))))
                                                                 (2ndtierdegrees cf-viable-notes 
                                                                                 cf-list 
                                                                                 number-of-measures 
                                                                                 voice-list
                                                                                 clef-ambitus
                                                                                 tonic-class))
                                                         (random (length (remove (lambda (x) (member x (cond ((assoc (+ (length cf-list) 1) 
                                                                                                                     bad-degrees) => 
                                                                                                                     (lambda (x) (cdr x)))
                                                                                                             (else '()))))
                                                                                 (2ndtierdegrees cf-viable-notes 
                                                                                                 cf-list 
                                                                                                 number-of-measures 
                                                                                                 voice-list
                                                                                                 clef-ambitus
                                                                                                 tonic-class)))))
                                               duration)))
                           bad-degrees))
            (else (display "sub...")
                  (display cf-list)
                  (newline)
		  (make-cf-iter 1 
                                (drop-right cf-list 1)
                                (remove (lambda (x) (> (first x) (length cf-list)))
                                        (cond ((null? (filter (lambda (x) (= (first x) (length cf-list)))
                                                              bad-degrees))
                                               (append bad-degrees (list (list (length cf-list)
                                                                               (car (last cf-list))))))
                                              (else (append (remove (lambda (x) (= (first x) (length cf-list)))
                                                                    bad-degrees)
                                                            (list (append (assoc (length cf-list) bad-degrees) 
                                                                          (list (car (last cf-list)))))))))))))
    (make-cf-iter 1 '() '())))

;checks to see if cp forms acceptable simultaneities with cf in terms of intervals, excluding first, penult, and ult,
(define (goodsimultaneities? checked-degrees cd-ambitus degrees-checked-against dca-ambitus root-class number-of-measures)
  (let ((checked-midinums (degrees-to-midinums checked-degrees cd-ambitus root-class))
        (midinums-checked-against (degrees-to-midinums degrees-checked-against dca-ambitus root-class)))
    (define (goodsimultaneities-iter? checked-midinums midinums-checked-against note-number)
      (cond ((null? checked-midinums) #t)
            ((or (and (or (= note-number 1)
                          (= note-number number-of-measures))
                      (or (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 0)
                          (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 3)
                          (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 4)
                          (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 7)))
                 (or (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 3)
                     (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 4)
                     (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 7)
                     (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 8)
                     (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 9)))
             (goodsimultaneities-iter? (rest checked-midinums) (rest midinums-checked-against) (+ note-number 1)))
            (else #f)))
    (goodsimultaneities-iter? checked-midinums midinums-checked-against 1)))

;checks for open, hidden, or intermittent parallels, allowing for horn 5ths and 8ths
(define (noparallel8vesor5ths? checked-degrees cd-ambitus degrees-checked-against dca-ambitus root-class)
  (let ((checked-midinums (degrees-to-midinums checked-degrees cd-ambitus root-class))
        (midinums-checked-against (degrees-to-midinums degrees-checked-against dca-ambitus root-class)))
    (define (noparallel8vesor5ths-iter? checked-midinums midinums-checked-against)
      (cond ((<= (length checked-midinums) 1) #t)
            ((and (or (and (> (car (first checked-midinums)) (car (first midinums-checked-against)))
                           (= (abs (- (car (first checked-midinums)) (car (second checked-midinums)))) 1))
                      (and (> (car (first midinums-checked-against)) (car (first checked-midinums)))
                           (= (abs (- (car (first midinums-checked-against)) (car (second midinums-checked-against)))) 1)))
                  (or (and (or (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 3)
                               (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 4)
                               (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 8)
                               (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 9)
                               (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 0))
                           (= (modulo (abs (- (car (second checked-midinums)) (car (second midinums-checked-against)))) 12) 7))
                      (and (= (modulo (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12) 7)
                           (or (= (modulo (abs (- (car (second checked-midinums)) (car (second midinums-checked-against)))) 12) 3)
                               (= (modulo (abs (- (car (second checked-midinums)) (car (second midinums-checked-against)))) 12) 4)
                               (= (modulo (abs (- (car (second checked-midinums)) (car (second midinums-checked-against)))) 12) 8)
                               (= (modulo (abs (- (car (second checked-midinums)) (car (second midinums-checked-against)))) 12) 9)
                               (= (modulo (abs (- (car (second checked-midinums)) (car (second midinums-checked-against)))) 12) 0)))))
             (noparallel8vesor5ths-iter? (rest checked-midinums) (rest midinums-checked-against)))
            ((and (or (and (< (car (first checked-midinums)) (car (second checked-midinums)))
                           (< (car (first midinums-checked-against)) (car (second midinums-checked-against))))
                      (and (> (car (first checked-midinums)) (car (second checked-midinums)))
                           (> (car (first midinums-checked-against)) (car (second midinums-checked-against)))))
                  (or (= (modulo (abs (- (car (second checked-midinums)) (car (second midinums-checked-against)))) 12) 7)
                      (= (modulo (abs (- (car (second checked-midinums)) (car (second midinums-checked-against)))) 12) 0))) #f)
            ((and (>= (length checked-midinums) 3)
                  (or (and (= (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 0)
                           (= (abs (- (car (third checked-midinums)) (car (third midinums-checked-against)))) 0))
                      (and (= (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 7)
                           (= (abs (- (car (third checked-midinums)) (car (third midinums-checked-against)))) 7))
                      (and (= (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 12)
                           (= (abs (- (car (third checked-midinums)) (car (third midinums-checked-against)))) 12))
                      (and (= (abs (- (car (first checked-midinums)) (car (first midinums-checked-against)))) 19)
                           (= (abs (- (car (third checked-midinums)) (car (third midinums-checked-against)))) 19)))) #f)
            (else (noparallel8vesor5ths-iter? (rest checked-midinums) (rest midinums-checked-against)))))
    (noparallel8vesor5ths-iter? checked-midinums midinums-checked-against)))

;makes sure both voices stays in place according to the voice list
(define (novoicecrossing? checked-degrees cd-ambitus degrees-checked-against dca-ambitus root-class cd-voice-list)
  (let ((checked-midinums (degrees-to-midinums checked-degrees cd-ambitus root-class))
        (midinums-checked-against (degrees-to-midinums degrees-checked-against dca-ambitus root-class)))
    (define (novoicecrossing-inner? checked-midinums midinums-checked-against)
      (cond ((null? checked-midinums) #t)
            ((or (and (= (first cd-voice-list) (last (sort cd-voice-list <)))
                      (< (car (first checked-midinums)) (car (first midinums-checked-against))))
                 (and (= (first cd-voice-list) (first (sort cd-voice-list <)))
                      (> (car (first checked-midinums)) (car (first midinums-checked-against)))))
             (novoicecrossing-inner? (rest checked-midinums) (rest midinums-checked-against)))
            (else #f)))
    (novoicecrossing-inner? checked-midinums midinums-checked-against)))

;checks that only contrary motion is used, to be placed only in first tier so that a strong preference exists for contrary motion
(define (noparallelmotion? checked-degrees cd-ambitus degrees-checked-against dca-ambitus root-class)
  (let ((checked-midinums (degrees-to-midinums checked-degrees cd-ambitus root-class))
        (midinums-checked-against (degrees-to-midinums degrees-checked-against dca-ambitus root-class)))
    (define (noparallelmotion-inner? checked-midinums midinums-checked-against)
      (cond ((<= (length checked-midinums) 1) #t)
            ((and (> (- (car (first checked-midinums)) (car (second checked-midinums))) 0)
                  (> (- (car (first midinums-checked-against)) (car (second midinums-checked-against))) 0)) #f)
            ((and (< (- (car (first checked-midinums)) (car (second checked-midinums))) 0)
                  (< (- (car (first midinums-checked-against)) (car (second midinums-checked-against))) 0)) #f)
            (else (noparallelmotion-inner? (rest checked-midinums) (rest midinums-checked-against)))))
    (noparallelmotion-inner? checked-midinums midinums-checked-against)))

;this procedure writes the counterpoint to the cantus firmus
(define (make-cp clef-ambitus tonic-class number-of-measures voice-list cf-list cf-ambitus)
  (let ((cp-viable-notes (assign-key clef-ambitus tonic-class)))
    (define (make-cp-iter duration cp-list bad-degrees)
      (cond ((and (= (length cp-list) number-of-measures)
                  (2ndtiercpconstraints? cp-list number-of-measures voice-list clef-ambitus cf-list cf-ambitus tonic-class))
             cp-list)
            ((> (length (remove (lambda (x) (member x (cond ((assoc (+ (length cp-list) 
                                                                       1) 
                                                                    bad-degrees) => 
                                                                                 (lambda (x) (cdr x)))
                                                            (else '()))))
                                (wellformedcpdegrees cp-viable-notes 
                                                     cp-list 
                                                     number-of-measures 
                                                     voice-list clef-ambitus cf-list cf-ambitus tonic-class)))
                0)
	     (display (+ 1 (length cp-list)))
             (display " bad-degrees: ")
             (display bad-degrees)
             (display " cp-list: ")
             (display cp-list)
             (newline)
             (make-cp-iter 1
                           (append cp-list 
                                   (list (cons (list-ref (remove (lambda (x) (member x (cond ((assoc (+ (length cp-list) 
                                                                                                        1) 
                                                                                                     bad-degrees) => 
                                                                                                     (lambda (x) (cdr x)))
                                                                                             (else '()))))
                                                                 (wellformedcpdegrees cp-viable-notes 
                                                                                      cp-list 
                                                                                      number-of-measures 
                                                                                      voice-list clef-ambitus cf-list cf-ambitus tonic-class)) 
                                                         (random (length (remove (lambda (x) (member x (cond ((assoc (+ (length cp-list) 1) 
                                                                                                                     bad-degrees) => 
                                                                                                                     (lambda (x) (cdr x)))
                                                                                                             (else '()))))
                                                                                 (wellformedcpdegrees cp-viable-notes 
                                                                                                      cp-list 
                                                                                                      number-of-measures 
                                                                                                      voice-list clef-ambitus cf-list cf-ambitus tonic-class)))))
                                               duration)))
                           bad-degrees))
            ((> (length (remove (lambda (x) (member x (cond ((assoc (+ (length cp-list) 
                                                                       1) 
                                                                    bad-degrees) => 
                                                                                 (lambda (x) (cdr x)))
                                                            (else '()))))
                                (2ndtiercpdegrees cp-viable-notes 
                                                  cp-list 
                                                  number-of-measures 
                                                  voice-list clef-ambitus cf-list cf-ambitus tonic-class)))
                0)
	     (display (+ 1 (length cp-list)))
             (display " bad-degrees: ")
             (display bad-degrees)
             (display " cp-list: ")
             (display cp-list)
             (newline)
             (make-cp-iter 1
                           (append cp-list 
                                   (list (cons (list-ref (remove (lambda (x) (member x (cond ((assoc (+ (length cp-list) 
                                                                                                        1) 
                                                                                                     bad-degrees) => 
                                                                                                     (lambda (x) (cdr x)))
                                                                                             (else '()))))
                                                                 (2ndtiercpdegrees cp-viable-notes 
                                                                                   cp-list 
                                                                                   number-of-measures 
                                                                                   voice-list clef-ambitus cf-list cf-ambitus tonic-class))
                                                         (random (length (remove (lambda (x) (member x (cond ((assoc (+ (length cp-list) 1) 
                                                                                                                     bad-degrees) => 
                                                                                                                     (lambda (x) (cdr x)))
                                                                                                             (else '()))))
                                                                                 (2ndtiercpdegrees cp-viable-notes 
                                                                                                   cp-list 
                                                                                                   number-of-measures 
                                                                                                   voice-list clef-ambitus cf-list cf-ambitus tonic-class)))))
                                               duration)))
                           bad-degrees))
;            ((and (empty? cp-list)
 ;                 (not (empty? bad-degrees)))
  ;           (set! cf (make-cf treble-ambitus 0 10 '(1 2)))
   ;          (make-cp soprano-ambitus 0 10 '(2 1) cf treble-ambitus))
            ((and (= (length cp-list) 0)
                  (= (length (remove (lambda (x) (member x (cond ((assoc (+ (length cp-list) 
                                                                       1) 
                                                                    bad-degrees) => 
                                                                                 (lambda (x) (cdr x)))
                                                            (else '()))))
                                (wellformedcpdegrees cp-viable-notes 
                                                     cp-list 
                                                     number-of-measures 
                                                     voice-list clef-ambitus cf-list cf-ambitus tonic-class)))
                     0)) #f)
            (else (display "sub...")
                  (display cp-list)
                  (newline)
		  (make-cp-iter 1 
                                (drop-right cp-list 1)
                                (remove (lambda (x) (> (first x) (length cp-list)))
                                        (cond ((null? (filter (lambda (x) (= (first x) (length cp-list)))
                                                              bad-degrees))
                                               (append bad-degrees (list (list (length cp-list)
                                                                               (car (last cp-list))))))
                                              (else (append (remove (lambda (x) (= (first x) (length cp-list)))
                                                                    bad-degrees)
                                                            (list (append (assoc (length cp-list) bad-degrees) 
                                                                          (list (car (last cp-list)))))))))))))
    (make-cp-iter 1 '() '())))

;meta-predicate including all cf predicates
(define (allcpconstraints? checked-notes number-of-measures voice-list cn-ambitus notes-checked-against nca-ambitus root-class)
  (cond ((and (first? checked-notes voice-list cn-ambitus root-class)
              (nobigbadleaps? checked-notes) 
              (tritoneless? checked-notes)
              (not (samedirtoolong? checked-notes))
              (balancedleaps? checked-notes)
              (nosixthleaps? checked-notes)
              (nooctaveleaps? checked-notes)
              (noadjtriadsor7thchords? checked-notes)
              (nonoterepetition? checked-notes)
              (rangewideenough? checked-notes)
              (nosequences? checked-notes)
              (no3consecutiveleaps? checked-notes)
              (penultimate? checked-notes number-of-measures voice-list)
              (ultimate? checked-notes number-of-measures voice-list)
              (goodsimultaneities? checked-notes cn-ambitus notes-checked-against nca-ambitus root-class number-of-measures)
              (noparallel8vesor5ths? checked-notes cn-ambitus notes-checked-against nca-ambitus root-class)
              (novoicecrossing? checked-notes cn-ambitus notes-checked-against nca-ambitus root-class voice-list)
              (noparallelmotion? checked-notes cn-ambitus notes-checked-against nca-ambitus root-class)) #t)
        (else #f)))

;meta-predicate including all cf predicates except 8ve and 6th leaps for 2nd tier options
(define (2ndtiercpconstraints? checked-notes number-of-measures voice-list cn-ambitus notes-checked-against nca-ambitus root-class)
  (cond ((and (first? checked-notes voice-list cn-ambitus root-class)
              (nobigbadleaps? checked-notes) 
              (tritoneless? checked-notes)
              (not (samedirtoolong? checked-notes))
              (balancedleaps? checked-notes)
              (noadjtriadsor7thchords? checked-notes)
              (nonoterepetition? checked-notes)
              (rangewideenough? checked-notes)
              (nosequences? checked-notes)
              (no3consecutiveleaps? checked-notes)
              (penultimate? checked-notes number-of-measures voice-list)
              (ultimate? checked-notes number-of-measures voice-list)
              (goodsimultaneities? checked-notes cn-ambitus notes-checked-against nca-ambitus root-class number-of-measures)
              (noparallel8vesor5ths? checked-notes cn-ambitus notes-checked-against nca-ambitus root-class)
              (novoicecrossing? checked-notes cn-ambitus notes-checked-against nca-ambitus root-class voice-list)) #t)
        (else #f)))

;makes list of all notes given a cf-list that are well formed as next degree
(define (wellformedcpdegrees cp-viable-notes cp-list number-of-measures voice-list cn-ambitus notes-checked-against nca-ambitus root-class)
  (define (wellformedcpdegrees-iter cp-viable-notes wellformedlist)
    (cond ((null? cp-viable-notes) wellformedlist)
          ((allcpconstraints? (append cp-list (list (cons (car (first cp-viable-notes)) 
                                                          1))) 
                              number-of-measures 
                              voice-list
                              cn-ambitus
                              notes-checked-against
                              nca-ambitus
                              root-class)
           (wellformedcpdegrees-iter (list-tail cp-viable-notes 1) (append wellformedlist (list (car (first cp-viable-notes))))))
          (else (wellformedcpdegrees-iter (list-tail cp-viable-notes 1) wellformedlist))))
  (wellformedcpdegrees-iter cp-viable-notes '()))
                                 
;makes list of all notes given a cf-list that are well formed as next degree, including "emergency", 2nd tier notes
(define (2ndtiercpdegrees cp-viable-notes cp-list number-of-measures voice-list cn-ambitus notes-checked-against nca-ambitus root-class)
  (define (2ndtiercpdegrees-iter cp-viable-notes wellformedlist)
    (cond ((null? cp-viable-notes) wellformedlist)
          ((2ndtiercpconstraints? (append cp-list (list (cons (car (first cp-viable-notes)) 
                                                          1))) 
                              number-of-measures 
                              voice-list
                              cn-ambitus
                              notes-checked-against
                              nca-ambitus
                              root-class)
           (2ndtiercpdegrees-iter (list-tail cp-viable-notes 1) (append wellformedlist (list (car (first cp-viable-notes))))))
          (else (2ndtiercpdegrees-iter (list-tail cp-viable-notes 1) wellformedlist))))
  (2ndtiercpdegrees-iter cp-viable-notes '()))  


;This procedure converts the degree numbers in the notelist to midinums, then that list to a string of lynotenames with durations
(define (degrees-to-ly notelist ambitus root)
  (let ((midinums (assign-key ambitus root)))
    (define (degrees-to-midinums-iter notelist newlist mn-index)
      (cond ((null? notelist) newlist)
            ((= (list-ref (map car midinums) mn-index) (first (map car notelist)))
             (degrees-to-midinums-iter (list-tail notelist 1)
                                       (append newlist (list (cons (list-ref (map cdr midinums) mn-index)
                                                                   (first (map cdr notelist))))) 0))
            (else (degrees-to-midinums-iter notelist newlist (+ mn-index 1)))))
    (define (midinums-to-notenames-iter notelist newstring)
      (cond ((null? notelist) newstring)
            (else (midinums-to-notenames-iter (list-tail notelist 1) (string-append newstring
                                                                                    (midi-to-ly 'sharp (first (map car notelist)))
                                                                                    (number->string (first (map cdr notelist)))
                                                                                    " ")))))
    (midinums-to-notenames-iter (degrees-to-midinums-iter notelist '() 0) "")))

(define (degrees-to-midinums notelist ambitus root)  
  (let ((midinums (assign-key ambitus root)))
    (define (degrees-to-midinums-iter notelist newlist mn-index)
      (cond ((null? notelist) newlist)
            ((= (list-ref (map car midinums) mn-index) (first (map car notelist)))
             (degrees-to-midinums-iter (list-tail notelist 1)
                                       (append newlist (list (cons (list-ref (map cdr midinums) mn-index)
                                                                   (first (map cdr notelist))))) 0))
            (else (degrees-to-midinums-iter notelist newlist (+ mn-index 1)))))
    (degrees-to-midinums-iter notelist '() 0)))


(define (write-ly root length cf-voicelist)
  (letrec ((cf (make-cf tenor-ambitus 0 8 '(3 4)))
           (cp (make-cp bass-ambitus 0 8 '(4 3) cf tenor-ambitus)))
    (cond ((false? cp) (write-ly root length cf-voicelist)))
    (define lystring (string-append "\\version \"2.12.3\"\ntop = {\n  \\clef tenor\n  \\key c \\major\n  \\time 4/4\n\n  "
                                    (degrees-to-ly cf tenor-ambitus 0)
                                    "\n}\n\nbottom = {\n  \\clef bass\n  \\key c \\major\n  \\time 4/4\n\n  "
                                    (degrees-to-ly cp bass-ambitus 0)

                                    "}\n\\score {\n  \\new StaffGroup <<\n    \\new Staff \\top\n    \\new Staff \\bottom\n  >>\n  \\layout { }\n  \\midi { }\n}\n"))
    (define lyfile (open-output-file "2vva.ly"))
    (display lystring lyfile)
    (close-output-port lyfile)))

(write-ly 0 8 '(3 4))

