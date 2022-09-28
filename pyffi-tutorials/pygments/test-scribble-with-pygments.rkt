#lang scribble/manual
@(require scriblib/render-cond scribble/core
          pyffi
          racket/format racket/string
          "sxml-renderer.rkt")
   
@(define hi     #f)
@(define python #f)
@(define html   #f)
@(define swift  #f)
@(define js     #f)
@(let ()
   ; Note: Using (let () ...) is important here.
   ;       If (begin ...) is used, then `scribble` moves definitions
   ;       from import-from above the initialization - which causes a crash.
   (initialize)
   (post-initialize)
   (import-from pygments            highlight)
   (import-from pygments.lexers     PythonLexer)
   (import-from pygments.lexers     HtmlLexer)
   (import-from pygments.lexers     SwiftLexer)
   (import-from pygments.lexers     JavascriptLexer)
   (import-from pygments.formatters HtmlFormatter)
   (set! hi (λ (lexer . code)
              (set! code (string-append* code))
              ; 
              (cond-element
               [html  (define h (pystring->string
                                 (highlight code lexer
                                            (HtmlFormatter #:noclasses #t))))
                      (html->element h)]
               [else ""])))
   (set! python (λ code (apply hi (PythonLexer)      code)))
   (set! html   (λ code (apply hi (HtmlLexer)        code)))
   (set! swift  (λ code (apply hi (SwiftLexer)       code)))
   (set! js     (λ code (apply hi (JavascriptLexer)  code))))
   

@bold{Python}
@python{def partition(l, r, nums):
        # Last element will be the pivot and the first element the pointer
        pivot, ptr = nums[r], l
        for i in range(l, r):
          if nums[i] <= pivot:
            # Swapping values smaller than the pivot to the front
            nums[i], nums[ptr] = nums[ptr], nums[i]
            ptr += 1
        # Finally swapping the last element with the pointer indexed number
        nums[ptr], nums[r] = nums[r], nums[ptr]
        return ptr}

@bold{HTML}
@html{<!DOCTYPE html>
      <html>
        <body>      
          <h1 class="large">A Header</h1>
          <p>A paragraph.</p>          
        </body>
      </html>}

@bold{Swift}
@swift{
private func partition(_ array: inout ArraySlice<Element>) 
             -> ArraySlice<Element>.Index {
  
  let midPoint = (array.startIndex + array.endIndex) / 2
  array.swapAt(midPoint, array.startIndex)
  let pivot = array[array.startIndex]
  var lower = array.startIndex
  var upper = array.endIndex - 1
  repeat {
    while lower < array.endIndex - 1 && array[lower] <= pivot {
      lower += 1
    }
    while array[upper] > pivot {
      upper -= 1
    }
    if lower < upper {
      array.swapAt(lower, upper)
    }
  } while lower < upper
  array.swapAt(array.startIndex, upper)
  return upper
}}

@bold{JavaScript}
@js{
function quickSort(arr, left = 0, right = arr.length - 1) {
  if (arr.length < 2) return arr;

  const index = partition(arr, left, right);

  if (left < index - 1) {
    quickSort(arr, left, index - 1)
  }
  if (right > index) {
    quickSort(arr, index, right)
  }
  return arr;
}}
