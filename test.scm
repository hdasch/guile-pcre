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

(format #t "~a\n" (pcre-exec (make-pcre "([[:digit:].]+)abc") "123.456abc789"))
(format #t "~a\n" (pcre-exec (make-pcre "abc") "123abc456"))
(format #t "UTF8: ~a\n" (pcre-config PCRE_CONFIG_UTF8))
(format #t "UTF16: ~a\n" (pcre-config PCRE_CONFIG_UTF16))
(format #t "JIT: ~a\n" (pcre-config PCRE_CONFIG_JIT))
(format #t "JITTARGET: ~a\n" (pcre-config PCRE_CONFIG_JITTARGET))
(format #t "MATCH_LIMIT: ~a\n" (pcre-config PCRE_CONFIG_MATCH_LIMIT))


(format #t "~a\n" (pcre-exec (make-pcre "abc" PCRE_CASELESS) "123abc456"))
(format #t "~a\n" (pcre-exec (make-pcre "abc" PCRE_CASELESS PCRE_STUDY_JIT_COMPILE) "123abc456"))
(format #t "~a\n" (pcre-exec (make-pcre "abc" PCRE_STUDY_JIT_COMPILE) "123abc456"))
