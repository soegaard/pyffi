#lang racket
;;;
;;; Run this file to configure `pyffi`.
;;;

;; The Python module `sysconfig` provides access to Python's configuration information.
;; If we can load the shared library `libpython3` then we can use `sysconfig` to
;; get all the information needed.

;; However, we need a way to figure out where `libpython3` is, before
;; we can load it.

;; The most reliable way is to run `python3` in a terminal and let
;; Python tell us where the shared library is.
;; In order to make it easy for users, this file will find `python3`,
;; run it, and extract the needed configuration information.

;; We are using `(find-executable-path "python3")` to find the path.
;; In the terminal where the environment variable PATH is set, this
;; works fine. However, if run in DrRacket (at least on macOS) the
;; environment variable PATH won't be set and we risk picking up a
;; different version.

;; As I am writing this, DrRacket will find the system installed
;; version of Python in /usr/bin/python3 which is version 3.8.9.
;; In the terminal we find version 3.10:
;;   /Library/Frameworks/Python.framework/Versions/3.10/bin/python3

;; That is:

;;   Run this configuration tool in an environment (terminal)
;;   where `python3` starts the Python command want to use.

;; After determining the location of the shared library
;; the location is written to the file "pyffi/path-to-libpython3.conf"
;; in the users preferences with `put-preferences`.


;; Notes:
;;  We run the command `python3 -m sysconfig` which writes
;;  the configuration information in groups of key/value pairs.
;;  The parser below parses the entire output (maybe we need
;;  more information at a later date).

(define system-configuration        #f)
(define system-configuration-string #f)


(define (get-configuration [given-path-to-python #f])
  (define path-to-python
    (or given-path-to-python
        (or (find-executable-path "python3")
            (find-executable-path "python")
            (find-executable-path "python3.10"))))

  (displayln "Configuration tool for `pyffi`.")
  (displayln "-------------------------------") 
  (displayln "This tool attempts to find the shared library `libpython3` ")
  (displayln "with the help of the `python3` executable.")
  (newline)
  (cond
    [path-to-python
     (displayln "The executable")
     (display   "    ")
     (displayln path-to-python)
     (displayln "will be used to find the location of the shared library.")]
    [else
     (displayln "The executable `python3` was not found in the current path.")
     (displayln "Double check that `python3` starts Python your terminal.")
     (displayln "Then run this configuration tool in the same terminal.")
     (exit 1)])
  (newline)

  (define success 'not-available-yet)
  (define str-path (if (path? path-to-python)
                       (path->string path-to-python)
                       path-to-python))
  (define command (string-append str-path " -m sysconfig"))
  (set! system-configuration-string
    (with-output-to-string
      (λ() (set! success (system command)))))
  (unless success
    (displayln "An error occurred while running the command:")
    (display   "    ")
    (displayln command)
    (displayln "Configuration of `pyffi` failed.")
    (exit 1))

  ; (displayln system-configuration-string)

  (define (string->lines x)
    (string-split x #rx"[\r\n]+"))
  (define (blank? x)
    (equal? (string-trim x) ""))
  (define (trim x)
    (string-trim (string-trim x) "\""))

  (define (string->key/value x)
    (and (string-contains? x ":")
         (match (regexp-match "([^:]*):(.*)" x)
           [(list full before after)
            (list (trim before) (trim after))])))

  ; parse the output of `python3 -m sysconfig` into an association list
  (define (parse info)
    (define lines (string->lines info))
    (let loop ([lines lines] [groups '()])
      (match lines
        ['()
         (reverse groups)]
        [(list* line lines)
         (if (blank? line)
             (loop (rest lines groups))
             (match (string->key/value line)
               [(list key value)
                (if (blank? value) ; new group?
                    (let ()
                      (define-values (group rest-lines) (parse-group line lines))
                      (loop rest-lines (cons group groups)))
                    (loop lines (cons (list key value) groups)))]))])))

  ; each group is parsed into a sub-association list
  (define (parse-group first-line lines)
    (define group-lines (takef lines (λ (x) (regexp-match "([^=]*)=(.*)" x))))
    (define rest-lines  (drop lines (length group-lines)))

    (define group
      (list (string-trim (trim first-line) ":")
            (for/list ([line group-lines])
              (match (regexp-match "([^=]*)=(.*)" line)
                [(list full before after)
                 (list (string-trim before)
                       (trim after))]))))
    (values group rest-lines))

  (set! system-configuration (parse system-configuration-string)))

;; Now the information is available in `system-configuration`.

(define (python-paths)
  (assoc  "Paths" system-configuration))

(define (python-variables)
  (assoc  "Variables" system-configuration))

(define (python-libdir)
  (define result (assoc "LIBDIR" (second (python-variables))))
  (and result (second result)))

(define (get-old-libdir)
  (get-preference 'pyffi:libdir (λ () #f)))

(define (set-new-libdir new-libdir-path)
  (define old (get-preference 'pyffi:libdir (λ () #f)))
  (unless (equal? old new-libdir-path)
    (put-preferences (list 'pyffi:libdir)
                     (list new-libdir-path)))
  (when old
    (displayln "The previous value of LIBDIR was:")
    (display   "    ")
    (displayln old))
  (displayln "The preference for LIBDIR is now set to:")
  (display   "    ")
  (displayln new-libdir-path))

(define (configure [path-to-python #f])
  (get-configuration path-to-python)
  (cond
    [(python-libdir) => set-new-libdir]
    [else
     (displayln "The LIBDIR key wasn't found.")
     (newline)
     (displayln "The sysconfiguration produced by the Python module `sysconfig` was:")
     (newline)
     (displayln system-configuration-string)]))

(module* main #f
  (command-line
   #:program     "configure-pyffi"
   #:args ([path-to-python #f])
   (configure path-to-python)))
