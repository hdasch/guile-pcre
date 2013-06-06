#!/home/hugh/guile/bin/guile -s
!#

(use-modules (ice-9 rdelim)
	     (ice-9 regex)
	     (ice-9 format)
	     (ice-9 match)
	     (rnrs control)
	     (srfi srfi-1)
	     (srfi srfi-69)
	     (guile-pcre)
	     (bohtner srfi-64))

(format #t "pcre Version: ~a\n" (pcre-version))
(format #t "~a\n" (pcre-exec (make-pcre "([[:digit:].]+)abc") "123.456abc789"))
(format #t "~a\n" (pcre-exec (make-pcre "abc") "123abc456"))

(format #t "~a\n" (pcre-exec (make-pcre "abc" PCRE_CASELESS) "123abc456"))
(format #t "~a\n" (pcre-exec (make-pcre "abc" PCRE_CASELESS PCRE_STUDY_JIT_COMPILE) "123abc456"))
(format #t "~a\n" (pcre-exec (make-pcre "abc" PCRE_STUDY_JIT_COMPILE) "123abc456"))
(let ((re (make-pcre "abc" PCRE_CASELESS)))
  (format #t "PCRE_INFO_BACKREFMAX: ~a\n" (pcre-fullinfo re PCRE_INFO_BACKREFMAX))
  (format #t "options: ~a, caseless: ~a\n" (pcre-fullinfo re PCRE_INFO_OPTIONS)
	  PCRE_CASELESS))


(test-begin "pcre-unit-test")
(test-assert "pcre?" (pcre? (make-pcre "([[:digit:].]+)abc")))
(test-assert "pcre-exec "
	     (pcre-exec (make-pcre "abc" PCRE_CASELESS) "123abc456"))

(test-eqv "single utf code"
	  1 (apply + (map (lambda (utf) (if (pcre-config utf) 1 0))
			  (list PCRE_CONFIG_UTF8
				PCRE_CONFIG_UTF16
				PCRE_CONFIG_UTF32))))
(test-assert "jit enabled" (boolean? (pcre-config PCRE_CONFIG_JIT)))
(test-assert "jit target" (string? (pcre-config PCRE_CONFIG_JITTARGET)))
(test-assert "match limit" (integer? (pcre-config PCRE_CONFIG_MATCH_LIMIT)))
(let* ((options PCRE_CASELESS)
       (re (make-pcre "abc" options)))
  (test-eq "caseless" options (pcre-fullinfo re PCRE_INFO_OPTIONS))
  (test-eq "backref" 0 (pcre-fullinfo re PCRE_INFO_BACKREFMAX)))
(test-end "pcre-unit-test")
