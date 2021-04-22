#! /usr/bin/env -S chibi-scheme -r -A ./ -I ${HOME}/work/projects/modules/self/schemeR7RS

(import (chibi)
	(only (lib test) c-e) ;; check-expect macro
	(only (lib parse)
		;; string -> (char -> bool)
		;; takes a string and returns a function that checks
		;; whether the given character is present in the string
		oneof

		;; string -> (char -> bool)
		;; takes a string containing an even number of characters
		;; and returns a function that checks whether a character
		;; is in that range
		range

		;; (char -> bool) -> (port -> null)
		;; takes a predicate and returns a function that skips
		;; characters in a port while the predicate returns
		;; true of them
		skip-while

		;; (char -> bool) -> (port -> list)
		;; takes a predicate and returns a function that reads
		;; characters while predicate holds
		read-while
	)

(define (negfn f) (lambda (x) (not (f x))) )
(define (invmap l x) (map (lambda (f) (f x)) l) )

;; 0A
;; A Sexp is a string following the grammar:
;; wsp = LF | SP | TAB
;; cmt = ';' *-LF
;; num = * 0..9
;; sym = *- ';'|wsp|num|'('|')'
;; sexp = *(?wsp ('(' sexp ')' | cmt | num | sym)) ?wsp

;; 0B
(define s0 "")
(define s1 "()")
(define s2 ";; 23")
(define s3 "908")
(define s4 "sym")
(define s5 "(234 sm (thingy 234))")
(define s6 "234\tsm\n()")
(define s7 "  \n\t \t  \n")

;; Wish list

;; 1 (wsp? c)
;; char -> bool
;; is the next character in a port whitespace?
(define wsp? (oneof "\t\n ") )

;; 2 (wsp? c)
(c-e (wsp? #\tab) ((oneof "\t\n ") #\tab) )
(c-e (wsp? #\newline) ((oneof "\t\n ") #\newline) )
(c-e (wsp? #\space) ((oneof "\t\n ") #\space) )
(c-e (wsp? #\x) ((oneof "\t\n ") #\x) )

;; 1 (cmt? c)
;; char -> bool
;; is the next character a comment character?
(define cmt? (oneof ";"))

;; 2 (cmt? c)
(c-e (cmt? #\;) ((oneof ";") #\;) )
(c-e (cmt? #\space) ((oneof ";") #\space) )

;; 1 (prn? c)
;; char -> bool
;; is the next character a list character?
(define prn? (oneof "()"))

;; 2 (prn? c)
(c-e (prn? #\() ((oneof "()") #\() )
(c-e (prn? #\)) ((oneof "()") #\)) )
(c-e (prn? #\x) ((oneof "()") #\x) )

;; 1 (num? c)
;; char -> bool
;; Is this a num character?
(define num? (range "09") )

;; 2 (num? c)
(c-e (num? #\0) ((range "09") #\0) )
(c-e (num? #\9) ((range "09") #\9) )
(c-e (num? #\5) ((range "09") #\5) )
(c-e (num? #\x) ((range "09") #\x) )

;; 1 (sym? c)
;; char -> bool
;; is this a sym character?
(define (orf x)
	(if (null? (cdr x)) (car x)
		(or (car x) (orf (cdr x)))) )
(define sym? (negfn (lambda (x) (orf (invmap `(,wsp? ,cmt? ,num? ,prn?) x)))) )

;; 2 (sym? c)
(c-e (sym? #\() #f )
(c-e (sym? #\)) #f )
(c-e (sym? #\5) #f )
(c-e (sym? #\space) #f )
(c-e (sym? #\s) #t)
	

;; 1 (wsp s)
;; port -> null
;; removes whitespace from an input port
(define wsp (skip-while wsp?) )

;; 2 (wsp s)
(c-e (wsp (open-input-string s0)) ((skip-while wsp?) (open-input-string s0)) )
(c-e (wsp (open-input-string s7)) ((skip-while wsp?) (open-input-string s7)) )

;; 1 (cmt p)
;; port -> null
;; skip comments
(define cmt (skip-while (negfn (oneof "\n"))) )

;; 2 (cmt s)
(c-e (cmt (open-input-string s2))
	((skip-while (negfn (oneof "\n"))) (open-input-string s2)) )

;; 1 (num p)
;; port -> number
;; read numbers from a port
(define (num p) (string->number (apply string ((read-while num?) p))) )

;; 2 (num p)
(c-e (num (open-input-string s0))
	(string->number (apply string ((read-while num?) (open-input-string s0)))) )

;; 1 (sym p)
;; port -> string
;; read symbols from a port
(define (sym p) (apply string ((read-while sym?) p)) )

;; 2 (sym p)
(c-e (sym (open-input-string s0)) (apply string ((read-while sym?) (open-input-string s0))) )


;; 1 (sexp s)
;; port -> sexp
;; takes an input port and returns a Scheme list
(define	(sexp p)
	(cond
		((eof-object? (peek-char p)) '() )
		((wsp? (peek-char p)) (wsp p) (sexp p) )
		((cmt? (peek-char p)) (cmt p) (sexp p) )
		(((oneof "(") (peek-char p)) (read-char p)
			(let ( (l (sexp p)) ) (cons l (sexp p)) ) )
		(((oneof ")") (peek-char p)) (read-char p) '() )
		((num? (peek-char p)) (let ( (n (num p)) ) (cons n (sexp p)) ) )
		((sym? (peek-char p)) (let ( (s (sym p)) ) (cons s (sexp p)) ) )) )

;; 2 (sexp s)
(c-e (sexp (open-input-string s0)) '())
(c-e (sexp (open-input-string s1)) '(()))
(c-e (sexp (open-input-string s5)) '((234 "sm" ("thingy" 234))))
(c-e (sexp (open-input-string "234\tsm\n()"))
	'(234 "sm" ()) )
(c-e (sexp (open-input-string "234(234 sm (thingy 234)\tsm)\n()"))
	'(234 (234 "sm" ("thingy" 234) "sm") ()) )

(define (main args)
	(display "")
	)
