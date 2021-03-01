#! /usr/bin/env -S chibi-scheme -r -A ./

(import (chibi) (chibi match)
	(scheme cxr) (only (scheme base) symbol=?)
	)

;; This parser uses an implementation of top-down operator precedence as described by Pratt.  It's my favourite parser implementation method.  Also the first one I actually understood.

;; 0A Data definition
;; An AST is a what???

;; A Sexp is a String with the following grammar

;; A Grammar is a what???

;; 0B Data examples

;; Wish list
;; ??? → ???
;; Returns next token
(define (tokens) #t )

;; Num Sexp → AST
;; Parser control loop.
(define (parser n s) '((meh))
	(token 'next)
	(token 'nud)
	(let lop ( (rbp (token 'rbp)) )
		(cond
			((< rbp (token 'lbp))
				(token 'next)
				(token 'led) )
			(else (token 'led) )) ) )



(define (main args)
	(dsp ""))