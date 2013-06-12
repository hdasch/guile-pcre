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
(format #t "libpcre configuration:\n")
(format #t "  PCRE_CONFIG_UTF8: ~a\n" (pcre-config PCRE_CONFIG_UTF8))
(format #t "  PCRE_CONFIG_UTF16: ~a\n" (pcre-config PCRE_CONFIG_UTF16))
(format #t "  PCRE_CONFIG_UTF32: ~a\n" (pcre-config PCRE_CONFIG_UTF32))
(format #t "  PCRE_CONFIG_UNICODE_PROPERTIES: ~a\n"
	(pcre-config PCRE_CONFIG_UNICODE_PROPERTIES))
(format #t "  PCRE_CONFIG_JIT: ~a\n" (pcre-config PCRE_CONFIG_JIT))
(format #t "  PCRE_CONFIG_BSR: ~a\n" (pcre-config PCRE_CONFIG_BSR))
(format #t "  PCRE_CONFIG_NEWLINE: ~a\n" (pcre-config PCRE_CONFIG_NEWLINE))
(format #t "  PCRE_CONFIG_LINK_SIZE: ~a\n" (pcre-config PCRE_CONFIG_LINK_SIZE))
(format #t "  PCRE_CONFIG_POSIX_MALLOC_THRESHOLD: ~a\n"
	(pcre-config PCRE_CONFIG_POSIX_MALLOC_THRESHOLD))
(format #t "  PCRE_CONFIG_STACKRECURSE: ~a\n"
	(pcre-config PCRE_CONFIG_STACKRECURSE))
(format #t "  PCRE_CONFIG_MATCH_LIMIT: ~a\n"
	(pcre-config PCRE_CONFIG_MATCH_LIMIT))
(format #t "  PCRE_CONFIG_MATCH_LIMIT_RECURSION: ~a\n"
	(pcre-config PCRE_CONFIG_MATCH_LIMIT_RECURSION))
(format #t "  PCRE_CONFIG_JITTARGET: ~a\n" (pcre-config PCRE_CONFIG_JITTARGET))


(test-begin "pcre-unit-test")
(test-assert "pcre?" (pcre? (make-pcre "([[:digit:].]+)abc")))
(test-assert "pcre-exec "
	     (pcre-exec (make-pcre "abc" PCRE_CASELESS) "123ABC456"))

(test-assert "caseless match "
	     (match
	       (pcre-exec (make-pcre "abc" PCRE_CASELESS) "123ABC456")
	       (#(string (3 . 6)) #t)
	       (_ #f)))
(test-assert "grouping match" 
	     (match
	       (pcre-exec (make-pcre
			   "(\\w+)\\s+(\\w+)\\s+(\\w+)\\s+(\\w+)\\s+(\\w+)")
			  "The quick brown fox jumped")
	       (#(string (0 . 26) (0 . 3) (4 . 9) (10 . 15) (16 . 19) (20 . 26)) #t)
	       (_ #f)))

(test-assert "jit compiled" 
	     (match
	       (pcre-exec (make-pcre
			   "(\\w+)\\s+(\\w+)\\s+(\\w+)\\s+(\\w+)\\s+(\\w+)"
			    PCRE_STUDY_JIT_COMPILE)
			  "The quick brown fox jumped")
	       (#(string (0 . 26) (0 . 3) (4 . 9) (10 . 15) (16 . 19) (20 . 26)) #t)
	       (_ #f)))

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
(test-assert "equalp"
	     (equal? (make-pcre
		      "/(?im)abc(?-i)d/" PCRE_EXTENDED)
		     (make-pcre
			 "/(?im)abc(?-i)d/"
			 PCRE_EXTENDED
			 PCRE_STUDY_JIT_COMPILE)))
(test-end "pcre-unit-test")
