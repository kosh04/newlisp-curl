#!newlisp

(load "curl.lsp")
(load "unittest.lsp")

(define-test "curl-version"
  (starts-with (curl-version) "libcurl"))

(define-test "curl-easy-escape"
  (= (curl-easy-escape "") "")
  (= (curl-easy-escape "newlisp.org/?q=index.html#123")
     "newlisp.org%2F%3Fq%3Dindex.html%23123"))

(define-test "curl-easy-unescape"
  (= (curl-easy-unescape ""))
  (= (curl-easy-unescape "newlisp.org%2F%3Fq%3Dindex.html%23123")
     "newlisp.org/?q=index.html#123"))

(define-test "curl-simple"
  (curl-simple "httpbin.org/ip"))

(define-test "curl-get#http"
  (json-parse (curl-get "http://httpbin.org/get")))

(define-test "curl-get#https"
  (json-parse (curl-get "https://httpbin.org/get")))

(Test:run)

