(define-module (bohtner srfi-64)
  #:export-syntax (test-assert
		   test-eqv
		   test-equal
		   test-eq
		   test-approximate
		   test-error
		   test-read-eval-string
		   test-begin
		   test-end
		   test-group
		   test-group-with-cleanup
		   test-runner?
		   test-runner-current
		   test-runner-get
		   test-runner-simple
		   test-runner-null
		   test-runner-create
		   test-runner-factory))

(include-from-path "bohtner/testing.scm")
