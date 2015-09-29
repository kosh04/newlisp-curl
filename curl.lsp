;;; curl.lsp

;; @module curl.lsp
;; @description libcurl wrapper
;; @version 0.3
;; @author KOBAYASHI Shigeru (kosh)
;; @license MIT
;; @location https://github.com/kosh04/newlisp-curl

;; ChangeLog:
;;
;; 2015-09-28  add "cdecl" to import calls (Thanks xytroxon, Lutz)
;; 2015-03-10  add testing
;; 2013-12-04  add function curl-get. add osx dylib.
;; 2011-08-02  first commit.

;; Link:
;;
;; libcurl - API
;; - http://curl.haxx.se/libcurl/c/
;; libcurl - source code examples
;; - http://curl.haxx.se/libcurl/c/example.html

(define libcurl
  (case ostype
    ("Win32"   "libcurl.dll")
    ("Windows" "libcurl.dll")   ; v.10.6.3+
    ("Cygwin"  "cygcurl-4.dll")
    ("BSD"     "libcurl.so")
    ("OSX"     "libcurl.dylib")
    ("Linux"   "libcurl.so.3")
    (true      "libcurl.so")    ; unix flavor
    ))

#include <curl/curl.h>
(import libcurl "curl_strequal" "cdecl")
(import libcurl "curl_strnequal" "cdecl")
(import libcurl "curl_formadd" "cdecl")
(import libcurl "curl_formget" "cdecl")
(import libcurl "curl_formfree" "cdecl")
(import libcurl "curl_getenv" "cdecl")
(import libcurl "curl_version" "cdecl")       ; char *curl_version();
(import libcurl "curl_easy_escape" "cdecl")   ; char *curl_easy_escape(CURL *curl, char *url, int length);
(import libcurl "curl_escape" "cdecl")        ; XXX (deprecated, do not use)
(import libcurl "curl_easy_unescape" "cdecl") ; char *curl_easy_unescape(CURL *curl, char *url, int inlength, int * outlength);
(import libcurl "curl_unescape" "cdecl")
(import libcurl "curl_free" "cdecl")          ; void curl_free(char *ptr);
(import libcurl "curl_global_init" "cdecl")   ; CURLcode curl_global_init(long flags);
(import libcurl "curl_global_init_mem" "cdecl")
(import libcurl "curl_global_cleanup" "cdecl")
(import libcurl "curl_slist_append" "cdecl")
(import libcurl "curl_slist_free_all" "cdecl")
(import libcurl "curl_getdate" "cdecl")
(import libcurl "curl_share_init" "cdecl")
(import libcurl "curl_share_setopt" "cdecl")
(import libcurl "curl_share_cleanup" "cdecl")
(import libcurl "curl_version_info" "cdecl")
(import libcurl "curl_easy_strerror" "cdecl") ; const char *curl_easy_strerror(CURLcode errornum);
(import libcurl "curl_share_strerror" "cdecl")
(import libcurl "curl_easy_pause" "cdecl")

#include <curl/easy.h>
(import libcurl "curl_easy_init" "cdecl")     ; CURL *curl_easy_init();
(import libcurl "curl_easy_setopt" "cdecl")   ; CURLcode curl_easy_setopt(CURL *handle, CURLoption option, parameter);
(import libcurl "curl_easy_perform" "cdecl")  ; CURLcode curl_easy_perform(CURL *handle);
(import libcurl "curl_easy_cleanup" "cdecl")  ; void curl_easy_cleanup(CURL * handle);
(import libcurl "curl_easy_getinfo" "cdecl")  ; CURLcode curl_easy_getinfo(CURL *curl, CURLINFO info, ...);
(import libcurl "curl_easy_duphandle" "cdecl")
(import libcurl "curl_easy_reset" "cdecl")
(import libcurl "curl_easy_recv" "cdecl")
(import libcurl "curl_easy_send" "cdecl")

#include <curl/mprintf.h>
(import libcurl "curl_mprintf" "cdecl")
(import libcurl "curl_mfprintf" "cdecl")
(import libcurl "curl_msprintf" "cdecl")
(import libcurl "curl_msnprintf" "cdecl")
(import libcurl "curl_mvprintf" "cdecl")
(import libcurl "curl_mvfprintf" "cdecl")
(import libcurl "curl_mvsprintf" "cdecl")
(import libcurl "curl_mvsnprintf" "cdecl")
(import libcurl "curl_maprintf" "cdecl")
(import libcurl "curl_mvaprintf" "cdecl")

#include <curl/multi.h>
(import libcurl "curl_multi_init" "cdecl")
(import libcurl "curl_multi_add_handle" "cdecl")
(import libcurl "curl_multi_remove_handle" "cdecl")
(import libcurl "curl_multi_fdset" "cdecl")
(import libcurl "curl_multi_perform" "cdecl")
(import libcurl "curl_multi_cleanup" "cdecl")
(import libcurl "curl_multi_info_read" "cdecl")
(import libcurl "curl_multi_strerror" "cdecl")
(import libcurl "curl_multi_socket" "cdecl")
(import libcurl "curl_multi_socket_action" "cdecl")
(import libcurl "curl_multi_socket_all" "cdecl")
(import libcurl "curl_multi_timeout" "cdecl")
(import libcurl "curl_multi_setopt" "cdecl")
(import libcurl "curl_multi_assign" "cdecl")

#include <curl/curl.h>
(define CURL_MAX_WRITE_SIZE 16384)

;; typedef enum { ... } CURLcode;
(define CURLE_OK 0)
(define CURLE_UNSUPPORTED_PROTOCOL 1)
(define CURLE_WRITE_ERROR 23)

(define CURL_ERROR_SIZE 256)

#define CURLOPTTYPE_LONG          0
#define CURLOPTTYPE_OBJECTPOINT   10000
#define CURLOPTTYPE_FUNCTIONPOINT 20000
#define CURLOPTTYPE_OFF_T         30000

;; typedef enum { ... } CURLoption;
(define CURLOPT_FILE (+ 10000 1))
(define CURLOPT_URL (+ 10000 2))
(define CURLOPT_PORT 3)
(define CURLOPT_INFILE (+ 10000 9))
(define CURLOPT_ERRORBUFFER (+ 10000 10))
(define CURLOPT_WRITEFUNCTION (+ 20000 11))
(define CURLOPT_POSTFIELDS (+ 10000 15))
(define CURLOPT_USERAGENT (+ 10000 18))
(define CURLOPT_HTTPHEADER (+ 10000 23))
(define CURLOPT_WRITEHEADER (+ 10000 29))

(define CURLOPT_VERBOSE 41)
(define CURLOPT_HEADER 42)
(define CURLOPT_NOPROGRESS 43)
(define CURLOPT_NOBODY 43)
(define CURLOPT_POST 47)

(define CURLOPT_FOLLOWLOCATION 52)
(define CURLOPT_SSL_VERIFYPEER 64)
(define CURLOPT_SSL_VERIFYHOST 81)

(define SSL_VERIFYHOST 81)

(define CURLOPT_WRITEDATA CURLOPT_FILE)
(define CURLOPT_READDATA  CURLOPT_INFILE)
(define CURLOPT_HEADERDATA CURLOPT_WRITEHEADER)

(define CURL_GLOBAL_SSL (<< 1 0))
(define CURL_GLOBAL_WIN32 (<< 1 1))
(define CURL_GLOBAL_ALL (| CURL_GLOBAL_SSL CURL_GLOBAL_WIN32))
(define CURL_GLOBAL_NOTHING 0)
(define CURL_GLOBAL_DEFAULT CURL_GLOBAL_ALL)

(define (curl--getstring ptr (len 0))
  (first (unpack (format "s%u" len) ptr)))

;; @syntax (curl-version)
;; @return libcurl version string
;; @example
;; (curl-version) => "libcurl/7.30.0 SecureTransport zlib/1.2.5"

(define (curl-version)
  (get-string (curl_version)))

;; @syntax (curl-easy-escape <string>)
;; @return URL encoded string
;; @example
;; (curl-easy-escape "newlisp.org/?q=index.html#123")
;; => "newlisp.org%2F%3Fq%3Dindex.html%23123"

(define (curl-easy-escape url)
  (letn ((curl (curl_easy_init))
         (res (curl_easy_escape curl url 0))
         (str (get-string res)))
    (curl_free res)
    (curl_easy_cleanup curl)
    str))

;; @syntax (curl-easy-unescape <string>)
;; @return URL decoded string
;; @example
;; (curl-easy-unescape "newlisp.org%2F%3Fq%3Dindex.html%23123")
;; => "newlisp.org/?q=index.html#123"

(define (curl-easy-unescape url)
  (letn ((curl (curl_easy_init))
         (outlen (pack "lu" 0))
         (res (curl_easy_unescape curl url 0 outlen))
         (str (curl--getstring res (get-int outlen))))
    (curl_free res)
    (curl_easy_cleanup curl)
    str))

;; @syntax (curl-simple <url>)
;; @example
;; (curl-simple "https://www.google.com/")
;; -> print html data to stdout
;; (curl-simple url) ~= (print (curl-get url))

(define (curl-simple url (verbose nil))
  (local (curl res)
    (setq curl (curl_easy_init))
    (when (!= curl 0)
      (curl_easy_setopt curl CURLOPT_URL url)
      (curl_easy_setopt curl CURLOPT_VERBOSE (if verbose 1 0))
      ;;(curl_easy_setopt curl CURLOPT_HEADER 0)
      (curl_easy_setopt curl CURLOPT_SSL_VERIFYPEER 0)
      ;;(curl_easy_setopt curl CURLOPT_SSL_VERIFYHOST 0)
      (setf res (curl_easy_perform curl))
      (curl_easy_cleanup curl)
      (if (!= res CURLE_OK)
          (write-line 2 (get-string (curl_easy_strerror res))))
      (= res CURLE_OK))))

;; @syntax (curl-get <url>)
;; @return recieved html data
;; @example
;; (curl-get "https://www.google.com/")
;; => "<HTML><HEAD><meta http-equiv=\"content-type\" ..."

(define (curl-get url)
  (local (curl buffer writefn res)
    (curl_global_init CURL_GLOBAL_ALL)
    (setq curl (curl_easy_init))
    (when (!= curl 0)
      (curl_easy_setopt curl CURLOPT_URL url)
      (curl_easy_setopt curl CURLOPT_USERAGENT "Mozilla/5.0")
      (curl_easy_setopt curl CURLOPT_FOLLOWLOCATION 0)
      (curl_easy_setopt curl CURLOPT_NOPROGRESS 1)
      (curl_easy_setopt curl CURLOPT_SSL_VERIFYPEER 0) ; option -k/--insecure
      ;;(curl_easy_setopt curl CURLOPT_SSL_VERIFYHOST 0)
      (setq buffer "")
      (setq writefn (lambda (buf size n data)
                      (extend buffer (curl--getstring buf (* size n)))
                      (* size n)))
      (curl_easy_setopt curl CURLOPT_WRITEFUNCTION (callback 0 'writefn))
      (setq res (curl_easy_perform curl))
      (when (!= res CURLE_OK)
        (setq buffer nil)
        (write-line 2 (get-string (curl_easy_strerror res))))
      (curl_easy_cleanup curl))
    (curl_global_cleanup)
    buffer))

(context MAIN)
