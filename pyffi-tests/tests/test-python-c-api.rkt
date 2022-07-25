#lang racket

(require pyffi/python-c-api
         pyffi/python-initialization
         pyffi/python-environment
         pyffi/python-evaluation
         pyffi/python-types)

(set-environment-variables)
(Py_Initialize)
         
;;; Informative functions:
(map displayln 
 (list (list 'is-initialized? (Py_IsInitialized))
       (list 'version         (Py_GetVersion))
       (list 'platform        (Py_GetPlatform))
       (list 'compiler        (Py_GetCompiler))
       (list 'build-info      (Py_GetBuildInfo))
       (list 'copyright       (Py_GetCopyright))))


;;;
;;; Integer Objects
;;;

; These all return 256 (or 256.0).
(PyLong_AsLong (PyLong_FromLong 256))
(PyLong_AsLong (PyLong_FromLongLong 256))
(PyLong_AsLong (PyLong_FromUnsignedLongLong 256))
(PyLong_AsLong (PyLong_FromDouble 256.0))
(PyLong_AsLong (PyLong_FromString "256" #f 10))
(PyLong_AsLongLong     (PyLong_FromLong 256))
(PyLong_AsUnsignedLong (PyLong_FromLong 256))
(PyLong_AsUnsignedLongLong (PyLong_FromLong 256))
(PyLong_AsDouble (PyLong_FromLong 256))
(PyLong_AsLong (PyLong_FromVoidPtr (PyLong_AsVoidPtr
                                    (PyLong_FromLong 256))))

;;;
;;; List Objects
;;;

; (PyList_New 5)

; These all evaluate to #t
(equal? (PyList_Size (PyList_New 5))
        5)
(equal? (let ()
          (define l (PyList_New 5))
          (PyList_SetItem l 0 (PyLong_FromLong 256))
          (PyLong_AsLong (PyList_GetItem l 0)))
        256)
(equal? (let ()
          (define l (PyList_New 5))
          (PyList_Insert l 0 (PyLong_FromLong 256))
          (PyList_Insert l 0 (PyLong_FromLong 255))
          (PyList_Insert l 0 (PyLong_FromLong 254))
          (list (PyLong_AsLong (PyList_GetItem l 0))
                (PyLong_AsLong (PyList_GetItem l 1))
                (PyLong_AsLong (PyList_GetItem l 2))))
        '(254 255 256))
(equal? (let ()
          (define l (PyList_New 0))
          (PyList_Append l (PyLong_FromLong 254))
          (PyList_Append l (PyLong_FromLong 255))
          (PyList_Append l (PyLong_FromLong 256))
          (list (PyLong_AsLong (PyList_GetItem l 0))
                (PyLong_AsLong (PyList_GetItem l 1))
                (PyLong_AsLong (PyList_GetItem l 2))))
        '(254 255 256))
(equal? (let ()
          (define l (PyList_New 0))
          (PyList_Append l (PyLong_FromLong 254))
          (PyList_Append l (PyLong_FromLong 255))
          (PyList_Append l (PyLong_FromLong 256))
          (PyList_Append l (PyLong_FromLong 257))
          (PyList_Append l (PyLong_FromLong 258))
          (PyList_Append l (PyLong_FromLong 259))
          (set! l (PyList_GetSlice l 1 3))
          (list (PyLong_AsLong (PyList_GetItem l 0))
                (PyLong_AsLong (PyList_GetItem l 1))))
        '(255 256))
(equal? (let ()
          (define l (PyList_New 0))
          (PyList_Append l (PyLong_FromLong 100))
          (PyList_Append l (PyLong_FromLong 101))
          (PyList_Append l (PyLong_FromLong 102))
          (PyList_Append l (PyLong_FromLong 103))
          (PyList_Append l (PyLong_FromLong 104))
          (PyList_Append l (PyLong_FromLong 105))
          (define m (PyList_New 0))
          (PyList_Append m (PyLong_FromLong 200))
          (PyList_Append m (PyLong_FromLong 201))
          (PyList_Append m (PyLong_FromLong 202))
          (PyList_SetSlice l 2 3 m)
          (for/list ([i 6])
            (PyLong_AsLong (PyList_GetItem l i))))
        '(100 101 200 201 202 103))
(equal? (let ()
          (define l (PyList_New 0))
          (PyList_Append l (PyLong_FromLong 190))
          (PyList_Append l (PyLong_FromLong 181))
          (PyList_Append l (PyLong_FromLong 172))
          (PyList_Append l (PyLong_FromLong 163))
          (PyList_Append l (PyLong_FromLong 154))
          (PyList_Append l (PyLong_FromLong 145))
          (PyList_Sort l)
          (for/list ([i 6])
            (PyLong_AsLong (PyList_GetItem l i))))
        '(145 154 163 172 181 190))

(initialize)
(finish-initialization)

(void (run* "import builtins"))
(displayln "b") (newline)
(define b (PyDict_GetItemString globals "builtins"))
(displayln (python->racket (PyObject_Repr b)))

(displayln "d") (newline)
(define d  (PyModule_GetDict b))
(void (python->racket (PyObject_Repr d)))
(python->racket (PyDict_GetItemString d "repr"))
