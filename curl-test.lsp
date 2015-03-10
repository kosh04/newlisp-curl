#!newlisp

(load "curl.lsp")
(load "unittest.lsp")

(define-test "curl-version"
  (starts-with (curl-version) "libcurl"))

(define-test "curl-simple"
  (curl-simple "httpbin.org/ip"))

(define-test "curl-get#http"
  (json-parse (curl-get "http://httpbin.org/get")))

(define-test "curl-get#https"
  (json-parse (curl-get "https://httpbin.org/get")))

(Test:run)

