(define-module (guile-pcre)
  #:export (make-pcre
	    pcre-exec))


(load-extension "libguile-pcre" "init_pcre")

(define (make-pcre pattern)
  (pcre-compile pattern))

