#!/home/hugh/guile/bin/guile -s
!#

(use-modules (ice-9 rdelim)
	     (ice-9 regex)
	     (ice-9 format)
	     (ice-9 match)
	     (rnrs control)
	     (srfi srfi-1)
	     (srfi srfi-69)
	     (guile-pcre))

;;;(set! %load-path (cons "/home/hugh/guile-pcre" %load-path)
;;;(load-extension "./libguile-pcre" "init_pcre")

(format #t "~a\n" (match-pcre (make-pcre "abc") "123abc456"))
