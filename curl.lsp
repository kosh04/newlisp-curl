;;; curl.lsp -- use libcurl

;; libcurl - API
;; - http://curl.haxx.se/libcurl/c/
;; libcurl - source code examples
;; - http://curl.haxx.se/libcurl/c/example.html
;;
;; 10 awesome things to do with cURL | CatsWhoCode.com
;; - http://www.catswhocode.com/blog/10-awesome-things-to-do-with-curl

(case ostype
  ("Win32"
;   (env "PATH" (append "C:\\tmp\\curl-7.21.7-devel-mingw32\\bin;" (env "PATH")))
   (define libcurl "libcurl.dll"))
  ("BSD"
   (define libcurl "libcurl.so"))
  (true
   (define libcurl "libcurl.so.3"))
  )

#include <curl/curl.h>
(import libcurl "curl_strequal")
(import libcurl "curl_strnequal")
(import libcurl "curl_formadd")
(import libcurl "curl_formget")
(import libcurl "curl_formfree")
(import libcurl "curl_getenv")
(import libcurl "curl_version")             ; char *curl_version();
(import libcurl "curl_easy_escape")         ; char *curl_easy_escape(CURL *curl, char *url, int length);
(import libcurl "curl_escape")              ; XXX (deprecated, do not use)
(import libcurl "curl_easy_unescape")       ; char *curl_easy_unescape(CURL *curl, char *url, int inlength, int * outlength);
(import libcurl "curl_unescape")
(import libcurl "curl_free")                ; void curl_free(char *ptr);
(import libcurl "curl_global_init")
(import libcurl "curl_global_init_mem")
(import libcurl "curl_global_cleanup")
(import libcurl "curl_slist_append")
(import libcurl "curl_slist_free_all")
(import libcurl "curl_getdate")
(import libcurl "curl_share_init")
(import libcurl "curl_share_setopt")
(import libcurl "curl_share_cleanup")
(import libcurl "curl_version_info")
(import libcurl "curl_easy_strerror")       ; const char *curl_easy_strerror(CURLcode errornum);
(import libcurl "curl_share_strerror")
(import libcurl "curl_easy_pause")

#include <curl/easy.h>
(import libcurl "curl_easy_init")           ; CURL *curl_easy_init();
(import libcurl "curl_easy_setopt")         ; CURLcode curl_easy_setopt(CURL *handle, CURLoption option, parameter);
(import libcurl "curl_easy_perform")        ; CURLcode curl_easy_perform(CURL *handle);
(import libcurl "curl_easy_cleanup")        ; void curl_easy_cleanup(CURL * handle);
(import libcurl "curl_easy_getinfo")        ; CURLcode curl_easy_getinfo(CURL *curl, CURLINFO info, ...);
(import libcurl "curl_easy_duphandle")
(import libcurl "curl_easy_reset")
(import libcurl "curl_easy_recv")
(import libcurl "curl_easy_send")

#include <curl/mprintf.h>
(import libcurl "curl_mprintf")
(import libcurl "curl_mfprintf")
(import libcurl "curl_msprintf")
(import libcurl "curl_msnprintf")
(import libcurl "curl_mvprintf")
(import libcurl "curl_mvfprintf")
(import libcurl "curl_mvsprintf")
(import libcurl "curl_mvsnprintf")
(import libcurl "curl_maprintf")
(import libcurl "curl_mvaprintf")

#include <curl/multi.h>
(import libcurl "curl_multi_init")
(import libcurl "curl_multi_add_handle")
(import libcurl "curl_multi_remove_handle")
(import libcurl "curl_multi_fdset")
(import libcurl "curl_multi_perform")
(import libcurl "curl_multi_cleanup")
(import libcurl "curl_multi_info_read")
(import libcurl "curl_multi_strerror")
(import libcurl "curl_multi_socket")
(import libcurl "curl_multi_socket_action")
(import libcurl "curl_multi_socket_all")
(import libcurl "curl_multi_timeout")
(import libcurl "curl_multi_setopt")
(import libcurl "curl_multi_assign")


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



(define (curl-version)
  (get-string (curl_version)))

(define (curl-easy-escape url (len 0))
  (letn ((curl (curl_easy_init))
         (res (curl_easy_escape curl url len))
         (str (get-string res)))
    (curl_free res)
    (curl_easy_cleanup curl)
    str))
;; (curl-easy-escape "newlisp.org/?q=index.html#123")
;; => "newlisp.org%2F%3Fq%3Dindex.html%23123"

(define (curl-easy-unescape url (len 0))
  (letn ((curl (curl_easy_init))
         (outlen (pack "lu" 0))
         (res (curl_easy_unescape curl url len outlen))
         (str (first (unpack (format "s%d" (get-int outlen)) res))))
    (curl_free res)
    (curl_easy_cleanup curl)
    str))

(define (curl-simple url)
  (local (curl res)
    (setq curl (curl_easy_init))
    (when (!= curl 0)
      (curl_easy_setopt curl CURLOPT_URL url)
      ;(curl_easy_setopt curl CURLOPT_HEADER 1)
      (setf res (curl_easy_perform curl))
      (if (!= res CURLE_OK)
          (println (get-string (curl_easy_strerror res))))
      (curl_easy_cleanup curl))))

(define (curl-https url)
  (local (curl res)
    (setq curl (curl_easy_init))
    (when (!= curl 0)
      (curl_easy_setopt curl CURLOPT_URL url)
;      (curl_easy_setopt curl CURLOPT_HEADER 1)
      (curl_easy_setopt curl CURLOPT_SSL_VERIFYPEER 0)
;      (curl_easy_setopt curl CURLOPT_SSL_VERIFYHOST 0)
      (setf res (curl_easy_perform curl))
      (if (!= res CURLE_OK)
          (println (get-string (curl_easy_strerror res))))
      ;; always cleanup
      (curl_easy_cleanup curl)
      res)))
;(curl-https "https://mail.google.com/mail/")

(define (curl-writefunction url)

  (define writeBuffer "")
  (define (recvf ptr size nmemb userp)
    ## fwrite(ptr, size, nmemb, (FILE *)userp);
    ;; (println (list ptr size nmemb userp))
    (let ((written (* size nmemb)))
      (extend writeBuffer (first (unpack (format "s%d" written) ptr)))
      (cpymem ptr (dup "x" written) written) ; XXX: dummy write
      ))

  ;(curl_global_init CURL_GLOBAL_ALL)
  (let ((curl (curl_easy_init))
        (ret 0))
    (if (= curl 0)
        (throw-error (string "curl_easy_init=" curl)))

    (curl_easy_setopt curl CURLOPT_URL url)
    (curl_easy_setopt curl CURLOPT_USERAGENT "Mozilla/5.0")
    (curl_easy_setopt curl CURLOPT_HEADER 0)
    (curl_easy_setopt curl CURLOPT_FOLLOWLOCATION 0)
    (curl_easy_setopt curl CURLOPT_NOPROGRESS 1)
    ;(curl_easy_setopt curl CURLOPT_POST 1)
    ;(curl_easy_setopt curl CURLOPT_POSTFIELDS "")

    ;(curl_easy_setopt curl CURLOPT_WRITEDATA str)
    (curl_easy_setopt curl CURLOPT_WRITEFUNCTION (callback 0 'recvf))
    ;(curl_easy_setopt curl CURLOPT_ERRORBUFFER errBuffer)

    (curl_easy_setopt curl CURLOPT_SSL_VERIFYPEER 0) ; option -k/--insecure
    ;(curl_easy_setopt curl CURLOPT_SSL_VERIFYHOST 0)
    ;(curl_easy_setopt curl CURLOPT_RETURNTRANSFER 1)

    (setf ret (curl_easy_perform curl))
    (if (!= ret CURLE_OK)
        (println (get-string (curl_easy_strerror ret)))
        (write 1 writeBuffer))

    (curl_easy_cleanup curl)
    ;(curl_global_cleanup)
    true))

(dolist (arg (main-args))
  (case arg
    ("-test"
     (curl-writefunction (or (main-args (+ $idx 1)) "localhost"))
     (exit))
    ))

(context MAIN)
