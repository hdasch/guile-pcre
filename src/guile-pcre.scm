(define-module (guile-pcre)
  #:export (make-pcre pcre-compile pcre-study pcre-exec pcre-config))

(eval-when
 (compile load eval)
 (load-extension "libguile-pcre" "init_pcre"))

(define-syntax make-helper
  (syntax-rules (PCRE_CASELESS
		 PCRE_MULTILINE
		 PCRE_DOTALL
		 PCRE_EXTENDED
		 PCRE_ANCHORED
		 PCRE_DOLLAR_ENDONLY
		 PCRE_EXTRA
		 PCRE_NOTBOL
		 PCRE_NOTEOL
		 PCRE_UNGREEDY
		 PCRE_NOTEMPTY
		 PCRE_UTF8
		 PCRE_UTF16
		 PCRE_UTF32
		 PCRE_NO_AUTO_CAPTURE
		 PCRE_NO_UTF8_CHECK
		 PCRE_NO_UTF16_CHECK
		 PCRE_NO_UTF32_CHECK
		 PCRE_AUTO_CALLOUT
		 PCRE_PARTIAL_SOFT
		 PCRE_PARTIAL
		 PCRE_DFA_SHORTEST
		 PCRE_DFA_RESTART
		 PCRE_FIRSTLINE
		 PCRE_DUPNAMES
		 PCRE_NEWLINE_CR
		 PCRE_NEWLINE_LF
		 PCRE_NEWLINE_CRLF
		 PCRE_NEWLINE_ANY
		 PCRE_NEWLINE_ANYCRLF
		 PCRE_BSR_ANYCRLF
		 PCRE_BSR_UNICODE
		 PCRE_JAVASCRIPT_COMPAT
		 PCRE_NO_START_OPTIMIZE
		 PCRE_NO_START_OPTIMISE
		 PCRE_PARTIAL_HARD
		 PCRE_NOTEMPTY_ATSTART
		 PCRE_UCP
		 PCRE_STUDY_JIT_COMPILE
		 PCRE_STUDY_JIT_PARTIAL_SOFT_COMPILE
		 PCRE_STUDY_JIT_PARTIAL_HARD_COMPILE
		 PCRE_STUDY_EXTRA_NEEDED)
    ((_ compile-flags study-flags pattern)
     (pcre-study (pcre-compile pattern compile-flags) study-flags))
    ((_ compile-flags study-flags pattern PCRE_CASELESS flags ...)
     (make-helper (logior compile-flags PCRE_CASELESS) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_MULTILINE flags ...)
     (make-helper (logior compile-flags PCRE_MULTILINE) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_DOTALL flags ...)
     (make-helper (logior compile-flags PCRE_DOTALL) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_EXTENDED flags ...)
     (make-helper (logior compile-flags PCRE_EXTENDED) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_ANCHORED flags ...)
     (make-helper (logior compile-flags PCRE_ANCHORED) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_DOLLAR_ENDONLY flags ...)
     (make-helper (logior compile-flags PCRE_DOLLAR_ENDONLY) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_EXTRA flags ...)
     (make-helper (logior compile-flags PCRE_EXTRA) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NOTBOL flags ...)
     (make-helper (logior compile-flags PCRE_NOTBOL) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NOTEOL flags ...)
     (make-helper (logior compile-flags PCRE_NOTEOL) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_UNGREEDY flags ...)
     (make-helper (logior compile-flags PCRE_UNGREEDY) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NOTEMPTY flags ...)
     (make-helper (logior compile-flags PCRE_NOTEMPTY) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_UTF8 flags ...)
     (make-helper (logior compile-flags PCRE_UTF8) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_UTF16 flags ...)
     (make-helper (logior compile-flags PCRE_UTF16) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_UTF32 flags ...)
     (make-helper (logior compile-flags PCRE_UTF32) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NO_AUTO_CAPTURE flags ...)
     (make-helper (logior compile-flags PCRE_NO_AUTO_CAPTURE) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NO_UTF8_CHECK flags ...)
     (make-helper (logior compile-flags PCRE_NO_UTF8_CHECK) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NO_UTF16_CHECK flags ...)
     (make-helper (logior compile-flags PCRE_NO_UTF16_CHECK) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NO_UTF32_CHECK flags ...)
     (make-helper (logior compile-flags PCRE_NO_UTF32_CHECK) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_AUTO_CALLOUT flags ...)
     (make-helper (logior compile-flags PCRE_AUTO_CALLOUT) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_PARTIAL_SOFT flags ...)
     (make-helper (logior compile-flags PCRE_PARTIAL_SOFT) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_PARTIAL flags ...)
     (make-helper (logior compile-flags PCRE_PARTIAL) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_DFA_SHORTEST flags ...)
     (make-helper (logior compile-flags PCRE_DFA_SHORTEST) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_DFA_RESTART flags ...)
     (make-helper (logior compile-flags PCRE_DFA_RESTART) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_FIRSTLINE flags ...)
     (make-helper (logior compile-flags PCRE_FIRSTLINE) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_DUPNAMES flags ...)
     (make-helper (logior compile-flags PCRE_DUPNAMES) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NEWLINE_CR flags ...)
     (make-helper (logior compile-flags PCRE_NEWLINE_CR) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NEWLINE_LF flags ...)
     (make-helper (logior compile-flags PCRE_NEWLINE_LF) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NEWLINE_CRLF flags ...)
     (make-helper (logior compile-flags PCRE_NEWLINE_CRLF) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NEWLINE_ANY flags ...)
     (make-helper (logior compile-flags PCRE_NEWLINE_ANY) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NEWLINE_ANYCRLF flags ...)
     (make-helper (logior compile-flags PCRE_NEWLINE_ANYCRLF) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_BSR_ANYCRLF flags ...)
     (make-helper (logior compile-flags PCRE_BSR_ANYCRLF) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_BSR_UNICODE flags ...)
     (make-helper (logior compile-flags PCRE_BSR_UNICODE) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_JAVASCRIPT_COMPAT flags ...)
     (make-helper (logior compile-flags PCRE_JAVASCRIPT_COMPAT) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NO_START_OPTIMIZE flags ...)
     (make-helper (logior compile-flags PCRE_NO_START_OPTIMIZE) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NO_START_OPTIMISE flags ...)
     (make-helper (logior compile-flags PCRE_NO_START_OPTIMISE) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_PARTIAL_HARD flags ...)
     (make-helper (logior compile-flags PCRE_PARTIAL_HARD) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_NOTEMPTY_ATSTART flags ...)
     (make-helper (logior compile-flags PCRE_NOTEMPTY_ATSTART) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_UCP flags ...)
     (make-helper (logior compile-flags PCRE_UCP) study-flags pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_STUDY_JIT_COMPILE flags ...)
     (make-helper compile-flags (logior study-flags PCRE_STUDY_JIT_COMPILE) pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_STUDY_JIT_PARTIAL_SOFT_COMPILE flags ...)
     (make-helper compile-flags (logior study-flags PCRE_STUDY_JIT_PARTIAL_SOFT_COMPILE) pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_STUDY_JIT_PARTIAL_HARD_COMPILE flags ...)
     (make-helper compile-flags (logior study-flags PCRE_STUDY_JIT_PARTIAL_HARD_COMPILE) pattern flags ...))
    ((_ compile-flags study-flags pattern PCRE_STUDY_EXTRA_NEEDED flags ...)
     (make-helper compile-flags (logior study-flags PCRE_STUDY_EXTRA_NEEDED) pattern flags ...))))

(define-syntax make-pcre
  (syntax-rules ()
    ((make-pcre pattern flags ...)
     (make-helper 0 0 pattern flags ...))))

(define (pcre-compile pattern . flags)
  (pcre-do-compile pattern flags))

