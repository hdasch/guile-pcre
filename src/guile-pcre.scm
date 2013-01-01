;;; installation: sudo mkdir /usr/lib/guile/2.0/extensions
;;; sudo ln -s ~hugh/guile-pcre/libguile-pcre.so  /usr/lib/guile/2.0/extensions
;;;sudo ln -s ~hugh/guile-pcre/guile-pcre.scm  /usr/share/guile/site/2.0/

;;; referenced as:
;(use-modules (guile-pcre))
;(format #t "~a\n" (match-pcre (make-pcre "abc") "123abc456"))

(define-module (guile-pcre)
  #:export (make-pcre pcre-exec))

(load-extension "libguile-pcre" "init_pcre")
