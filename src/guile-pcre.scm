(define-module (guile-pcre)
  #:export (make-pcre
	    pcre-exec))


(load-extension "libguile-pcre" "init_pcre")
