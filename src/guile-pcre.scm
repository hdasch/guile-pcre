(define-module (guile-pcre)
  #:export (make-pcre pcre-compile pcre-study pcre-exec pcre-config))

(load-extension "libguile-pcre" "init_pcre")

(define (make-pcre pattern . flags)
  (pcre-study (pcre-do-compile pattern flags)))

(define (pcre-compile pattern . flags)
  (pcre-do-compile pattern flags))

