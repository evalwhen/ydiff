;; ydiff - a language-aware tool for comparing programs
;; Copyright (C) 2011-2013 Yin Wang (yinwang0@gmail.com)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


#lang racket

(require "structs.rkt")
(require "utils.rkt")
(require "parsec.rkt")

(provide parse-lisp)


;-------------------------------------------------------------
;                     scanner setttings
;-------------------------------------------------------------

; single quote is considered a delimeter in s-expression

(define set-parameters
  (lambda ()
    (set-delims (list "("  ")"  "["  "]"  "{"  "}" "'"  "`"  "," ))
    (set-line-comment (list ";"))
    (set-comment-start "")
    (set-comment-end "")
    (set-operators  '())
    (set-quotation-marks '(#\"))
    (set-lisp-char (list "#\\" "?\\"))
    (set-significant-whitespaces '())))



;-------------------------------------------------------------
;                        parser
;-------------------------------------------------------------

(:: $open
     (@or (@~ "(") (@~ "[")))

(:: $close
     (@or (@~ ")") (@~ "]")))

(:: $non-parens
     (@and (@! $open) (@! $close)))

(::= $parens 'sexp
     (@seq $open (@* $sexp) $close))

(:: $sexp
    (@+ (@or $parens $non-parens)))

(:: $program $sexp)


(define parse-lisp
  (lambda (s)
    (set-parameters)
    (first-val ($eval $sexp (scan s)))))


;;; command line use only

;; (let ([args (current-command-line-arguments)])
;;   (cond
;;    [(null? args) '()]
;;    [else
;;     (set-parameters)
;;     (print (parse-lisp (read-file (vector-ref args 0))))]))








(define myp
  (lambda (s)
    (let-values (((res remain) (fun-lisp2 (scan s))))
                res)))

(define fun-lisp2
  (lambda (toks)
    (cond ((null? toks) (values #f #f))
          ((open? (first toks))
           (let-values (((child remain) (p-child (cdr toks) '())))
                       (cond
                        ((null? remain) (error "missing close paren"))
                        ;; ((not (null? (cdr remain))) (error "find trailing exprs"))
                        ((not (close? (first remain))) (error "last not close paren"))
                        (else (values (Node 'sexp (Node-start (first toks)) (Node-end (first remain)) child #f #f)
                                      remain)))))
          (else (error "must begin with paren")))))

(define p-child
  (lambda (remain res)
    (cond
     ((null? remain) (values (reverse res) remain))
     ((open? (first remain))
      (let-values (((res1 remain1) (fun-lisp2 remain)))
                  (p-child remain1 (cons res1 res))))
     ((close? (first remain))
      (values (reverse res) remain))
     (else
      (p-child (cdr remain) (cons (car remain) res))))))

(define open?
  (lambda (node)
          (string=? "(" (Node-elts node))))


(define close?
  (lambda (node)
          (string=? ")" (Node-elts node))))
