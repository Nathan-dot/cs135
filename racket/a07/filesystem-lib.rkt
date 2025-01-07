#lang racket

(provide display-fs)

;; A FileSystem is one of:
;; * File
;; * Dir

(define-struct file (name size owner))
;; A File is a (make-file Str Nat Sym)

(define-struct dir (name owner contents))
;; A Dir is a (make-dir Str Sym (listofFileSystem))

;; This is a dirty hack, please ignore
(define (student-struct->private-struct s)
  (local [(define v (struct->vector s))]
    (cond [(symbol=? (vector-ref v 0) 'struct:file)
           (file (vector-ref v 1) (vector-ref v 2) (vector-ref v 3))]
          [else
           (dir (vector-ref v 1) (vector-ref v 2) (map student-struct->private-struct
                                                       (vector-ref v 3)))])))


;; (display-fs fs) prints fs in a pretty manner
;; effects: text is printed to the screen

(define (display-fs fs)
  (local [;; (make-label lolast icon name) produces a string that ends with icon, name, and \n
          ;;   and that starts with the appropriate type of line based on lolast
          ;; make-label: (listof Bool) Str Str -> Str
          (define (make-label lolast icon name)
            (string-append (cond [(empty? lolast) ""]
                                 [(first lolast) "\u2514"]
                                 [else "\u251C"])
                           icon name "\n"))
          ;; (make-line/acc acc lolast) adds appropriate lines to acc, based on whether each level of
          ;;   indentation (recorded in lolast) is the last child (true) or not (false)
          ;; make-line/acc: Str (listof Bool) -> Str
          (define (make-line/acc acc lolast)
            (cond [(empty? lolast) acc]
                  [(first lolast) (make-line/acc (string-append " " acc) (rest lolast))]
                  [else   (make-line/acc (string-append "\u2502" acc) (rest lolast))]))
          ;; (make-line lolast icon name) produces an appropriate string for the given icon + label
          ;;   for each level of indentation recorded in lolast, a | is printed if that level
          ;;   is not the last child (i.e. that element of lolast is true)
          ;; make-line: (listof Bool) Str Str -> Str
          (define (make-line lolast icon name)
            (cond [(empty? lolast) (make-label lolast icon name)]
                  [else (make-line/acc (make-label lolast icon name) (rest lolast))]))
          ;; (fs->string fs lolast) returns a string representing fs and its children.
          ;;    the string is preprended with lines based on lolast.
          ;; fs->string:FileSystem (listof Bool) -> Str
          (define (fs->string fs lolast)
            (cond [(file? fs) (make-line lolast "\u25E6"  (file-name fs))]
                  [else (string-append (make-line lolast "\u25bc"  (dir-name fs))
                                       (file-systems->string (dir-contents fs) lolast))]))
          ;; (file-systems->string fs-list lolast) returns  string representing the forest
          ;;  fs-list (prepended according to lolast)
          ;; file-systems->string: (listofFileSystem) (listof Bool) -> Str
          (define (file-systems->string fs-list lolast)
            (cond [(empty? fs-list) ""]
                  [else (string-append (fs->string (first fs-list) (cons (empty? (rest fs-list))
                                                                         lolast))
                                       (file-systems->string (rest fs-list) lolast))]))]
         ;; Display prints a string literally, rather than using racket notation.  
         ;; e.g. "\n" actually makes a new line, etc.
         ;; This is a "side effect", meaning this is not a "pure" function anymore.
         ;; If we just produced the string, Dr.Racket would not display it in a user-friendly way.
         (display (fs->string (student-struct->private-struct fs) empty))))


