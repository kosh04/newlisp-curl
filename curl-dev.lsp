;;; curl-dev.lsp --- cURL using FOOP

;; (not worked yet)

;; sample:
;; - https://github.com/php-curl-class/php-curl-class

(load "curl.lsp")

(new Class 'Curl)

(context Curl)

(define (defslots vars)
  (define _vars vars)
  (map set vars (sequence 1 (length vars))))

(defslots
  '(@curl @id @error
    @onsuccess @onerror @oncomplete
    @headers @options
    ))

;; construct body
(define (init)
  (setf (self @curl) (curl_easy_init)
        ;;(self @id) nil
        (self @error) nil
        (self @error-code) nil
        (self @response) nil
        ;;...
        (self @onsuccess) nil
        (self @onerror) nil
        (self @oncomplete) nil
        ;;...
        (self @headers) '()
        (self @options) '())
  (:setopt (self) CURLOPT_USERAGENT (format "newLISP-Curl %s" ostype))
  (self))

;; construct
(define (Curl:Curl)
  (:init (cons (context) (dup nil (length _vars)))))

;; @syntax (:setopt <Curl> option value)
(define (setopt option value)
  ;;(setf ((self @options) @option) value)
  (curl_easy_setopt (self @curl) opt value))

;; @syntax (:getopt <Curl> option)
(define (getopt option)
  (self @options))

(define (call f)
  (when (callable? f)
    (apply f (args))))

(define (exec ch)
  (setf (self @curl-error-message) (curl_error (self @curl))
        (self @curl-error) ...
        (self @error) (or (self @curlError)
                          (self @httpError))
        (self @error-code) (if (!= (self @error) 0)
                               (if (!= (self @curlError) 0)
                                   (self @curlErrorCode)
                                   (self @httpStatusCode))
                               0)
        )
  (if (:getopt (self) CURLINFO_HEADER_OUT)
      (setf (self @request-headers)
            (:parse-request-headers
             (self)
             (curl_getinfo (self @curl))
             CURLINFO_HEADER_OUT)))

  ;; ...

  (if (self @error)
      (:call (self) (self @error-function))
      (:call (self) (self @success-function)))

  (:call (self) (self @complete-function))
  ;;(:call (self) (self @oncomplete))
  
  (self @response))

;;(:curl-error-message c)

(define (get url data)
  (:set-url (self) url data)
  (:setopt (self) CURLOPT_CUSTOMREQUEST "GET")
  (:setopt (self) CURLOPT_HTTPGET true)
  (:exec (self)))

;; @syntax (:onsuccess Curl callback)
(define (onsuccess f)
  ;;(setf (self @success-function) f)
  (setf (self @onsuccess) f)
  )

(context MAIN)


;;; Example:

(setq c (Curl))
(:get c "http://www.example.com/" (@ ("q" "keyword")))

(:post c "http://www.example.com/login"
       (@ ("username" "myusername")
          ("password" "mypassword")))

(:setopt c CURL_OPT_SSL_VERIFYPEER nil)
(:get c "https://ssl.example.com")

(:put c "http://api.example.com/user/"
      (@ ("first_name" "Zach")
         ("last_name" "Borboa")))

(:patch c "http://api.example.com/profile/"
        (@ ("image" "@path/to/file.jpg")))

(:patch c "http://api.example.com/profile/"
        (@ ("image" (CURLFile "@path/to/file.jpg"))))

(:delete c "http://api.example.com/user/"
         (@ ("id" "1234")))

(:setopt c CURLOPT_ENCODING "gzip")
(:download c "https://www.example.com/image.png" "/tmp/image.png")

(lookup "Content-Type" (:responseHeaders c))
;;((:responseHeaders c) "Content-Type")
(:responseHeaders c "Content-Type")     ;-> "image/png"

(:setopt c CURLOPT_USERAGENT "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1")

;; parallel
(setq mc (MultiCurl))

(:onsuccess mc (lambda (instance) ))
(:onerror mc (lambda (instance) ))
(:oncomplete mc (lambda (instance) ))

(:add-get mc "https://www.google.com/search" (@ ("q" "hello world")))
(:add-get mc "https://duckduckgo.com/"       (@ ("q" "hello world")))
(:add-get mc "https://www.bing.com/search"   (@ ("q" "hello world")))
(:start mc)

;;
