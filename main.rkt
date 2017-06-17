#lang racket/base

#|
Racket-md5-website
Copyright (C) 2017 Castiglia Vincenzo

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(require web-server/servlet
         web-server/servlet-env
         db
         "../racket-md5/md5.rkt")

;;DATABASE
(define c
  (virtual-connection
   (λ ()
     (postgresql-connect #:user "postgres"
                         #:database "md5"))))

;;when the program starts the table is created
(query-exec c "
CREATE TABLE IF NOT EXISTS md5 (
input TEXT NOT NULL,
hash VARCHAR(32) NOT NULL,
PRIMARY KEY (input, hash))")
(disconnect c)


(define (db-insert-md5 input hash)
  ;(set! hash (string-append hash "a")) ;;exception test
  (with-handlers ([exn:fail:sql?
                   (λ (e)
                     ;;unless it's a duplicate
                     (unless
                         (equal? (exn:fail:sql-sqlstate e) "23505")
                       (raise e)))])
    (query-exec c "INSERT INTO md5 VALUES ($1, $2)" input hash))
  (disconnect c))

(define (db-reverse-md5 input)
  (define out
    (query-list c "SELECT input FROM md5 WHERE hash = $1" input))
  (disconnect c)
  out)


;;WEB  
(define (start request)
  (site-dispatch request))

(define-values (site-dispatch site-url)
  (dispatch-rules
   
   (("")
    (λ (req)
      (render-home)))
   
   (("calculate")
    (λ (req)
      (define input (extract-from-request 'input req))
      (define output (md5 input))
      (printf "calculate input: ~a\n" input)
      (db-insert-md5 input output)
      (render-home #:calculated output)))
   
   (("reverse")
    (λ (req)
      (define input (extract-from-request 'input req))
      (printf "finding reverse of: ~a\n" input)
      (define out (db-reverse-md5 input))
      (render-home #:reversed out)))))


(define (extract-from-request symbol request)
  (extract-binding/single symbol (request-bindings request)))


(define (render-home
         #:calculated (calculated #f)
         #:reversed (reversed #f))
  ;(set! reversed (list "aaa" "abc")) ;;collision test
  (response/xexpr
   `(html
     (head
      (title "MD5 Castiglia Vincenzo")
      (link ((rel "stylesheet") (href "/main.css"))))
     (body
      (h1 "homepage")

      ,(if calculated
           `(span "calculated: " ,calculated) "")
      ,(if reversed 
           `(span "reversed:" `(br)
                  ,@(map (λ (str)  `(span ,str (br))) reversed)) "")
      
      (form ((action "calculate"))
            (h3 "calculate")
            (input ((name "input")))
            (input ((type "submit"))))
      
      (form ((action "reverse"))
            (h3 "reverse")
            (input ((name "input")))
            (input ((type "submit"))))))))


(define (404-response req)
  (response/xexpr #:code 404 #:message #"ERROR"
                  '(html (body (div "404 ERROR")))))


(serve/servlet
 start
 #:servlet-path "/"
 #:servlet-regexp #rx""
 #:server-root-path (current-directory)
 #:extra-files-paths (list (build-path (current-directory) "static"))
 #:file-not-found-responder 404-response
 #:port 8080)
