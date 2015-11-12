#!newlisp

(load "curl.lsp")
(load "unittest.lsp")

(define-test "curl-version"
  (starts-with (curl-version) "libcurl"))

(define-test "curl-easy-escape"
  (= (curl-easy-escape "") "")
  (= (curl-easy-escape "newlisp.org/?q=index.html#123")
     "newlisp.org%2F%3Fq%3Dindex.html%23123")
  (= (curl-easy-escape "\xE3\x82\xB3\xE3\x83\x9E\xE3\x83\xB3\xE3\x83\x89\xE3\x83\xBC")
     "%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89%E3%83%BC")
  )

(define-test "curl-easy-unescape"
  (= (curl-easy-unescape "") "")
  (= (curl-easy-unescape "newlisp.org%2F%3Fq%3Dindex.html%23123")
     "newlisp.org/?q=index.html#123")
  (= (curl-easy-unescape "%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89%E3%83%BC")
     "\xE3\x82\xB3\xE3\x83\x9E\xE3\x83\xB3\xE3\x83\x89\xE3\x83\xBC"))

(define-test "curl-simple"
  (curl-simple "httpbin.org/ip"))

(define-test "curl-get#http"
  (json-parse (curl-get "http://httpbin.org/get")))

(define-test "curl-get#https"
  (json-parse (curl-get "https://httpbin.org/get")))
