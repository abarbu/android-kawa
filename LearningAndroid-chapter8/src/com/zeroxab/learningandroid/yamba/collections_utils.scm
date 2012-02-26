;; Some handy functions for dealing with java.util.Collections.
;; Jamison Hope 2009-12-07 15:06:52EST jrh collections-utils.scm
;; Time-stamp: <2009-12-08 16:42:00EST jrh collections_utils.scm>
;; $Id: collections_utils.scm 160 2009-12-09 22:13:24Z jhope $
(module-export Enumeration->list
               Iterator->list
               Iterable->list
               Array->list
               ->list
               map-enumeration
               map-iterator
               map-iterable
               map-array
               for-each-enumeration
               for-each-iterator
               for-each-iterable
               for-each-array
               better-map
               better-for-each)

;;; Returns a list of the elements of the given Enumeration.
(define (Enumeration->list (enum :: java.util.Enumeration)) :: list
  (let loop ((ls '()))
    (if (enum:hasMoreElements)
        (loop (cons (enum:nextElement) ls))
        (reverse ls))))

;;; Returns a list of the elements of the given Iterator.
(define (Iterator->list (iter :: java.util.Iterator)) :: list
  (let loop ((ls '()))
    (if (iter:hasNext)
        (loop (cons (iter:next) ls))
        (reverse ls))))

;;; Returns a list of the elements of the given Iterable.
(define (Iterable->list (it :: java.lang.Iterable)) :: list
  (Iterator->list (it:iterator)))

;;; Returns a list of the elements of the given array.
(define (Array->list (array :: Object)) :: list
  (when (*:isArray (array:getClass))
        (let* ((n :: int (java.lang.reflect.Array:getLength array))
               (result-vector :: vector (make-vector n)))
          (do ((i 0 (+ i 1)))
              ((= i n) (vector->list result-vector))
            (vector-set! result-vector i (array i))))))

;;; Converts the given object into a list.
(define ->list
  (case-lambda
   ((l :: list) l)
   ((l :: java.lang.Iterable) (Iterable->list l))
   ((l :: java.util.Iterator) (Iterator->list l))
   ((l :: java.util.Enumeration) (Enumeration->list l))
   ((l :: Object) (Array->list l))))

;;; Map for an Enumeration
(define (map-enumeration (f :: procedure)
                         (enum :: java.util.Enumeration))
  :: list
  (let loop ((ls '()))
    (if (enum:hasMoreElements)
        (loop (cons (f (enum:nextElement)) ls))
        (reverse ls))))

;;; Map for an Iterator
(define (map-iterator (f :: procedure)
                      (iter :: java.util.Iterator))
  :: list
  (let loop ((ls '()))
    (if (iter:hasNext)
        (loop (cons (f (iter:next)) ls))
        (reverse ls))))

;;; Map for an Iterable
(define (map-iterable (f :: procedure)
                      (it :: java.lang.Iterable))
  :: list
  (map-iterator f (it:iterator)))

;;; Map for an Array
(define (map-array (f :: procedure)
                   (obj :: Object))
  :: list
  (when (*:isArray (obj:getClass))
        (let* ((n :: int (java.lang.reflect.Array:getLength obj))
               (result-vector :: vector (make-vector n)))
          (do ((i 0 (+ i 1)))
              ((= i n) (vector->list result-vector))
            (vector-set! result-vector i (f (obj i)))))))

;;; For-each for an Enumeration
(define (for-each-enumeration (f :: procedure)
                              (enum :: java.util.Enumeration))
  (let loop ()
    (when (enum:hasMoreElements)
          (f (enum:nextElement))
          (loop))))

;;; For-each for an Iterator
(define (for-each-iterator (f :: procedure)
                           (iter :: java.util.Iterator))
  (let loop ()
    (when (iter:hasNext)
          (f (iter:next))
          (loop))))

;;; For-each for an Iterable
(define (for-each-iterable (f :: procedure)
                           (it :: java.lang.Iterable))
  (for-each-iterator f (it:iterator)))

;;; For-each for an Array
(define (for-each-array (f :: procedure)
                        (obj :: Object))
  (when (*:isArray (obj:getClass))
        (let ((n :: int (java.lang.reflect.Array:getLength obj)))
          (do ((i 0 (+ i 1)))
              ((= i n))
            (f (obj i))))))

;;; A version of map which works with lists, Enumerations, Iterators,
;;; Iterables, and Arrays.
(define better-map
  (case-lambda
   (((f :: procedure) (l :: list))
    (map f l))
   (((f :: procedure) (l :: java.util.Enumeration))
    (map-enumeration f l))
   (((f :: procedure) (l :: java.util.Iterator))
    (map-iterator f l))
   (((f :: procedure) (l :: java.lang.Iterable))
    (map-iterable f l))
   (((f :: procedure) (l :: Object))
    (map-array f l))
   (((f :: procedure) (l1 :: Object) (l2 :: Object) . rest)
    (apply map f (append (map ->list `(,l1 ,l2))
                         (map ->list rest))))))

;;; A version of for-each which works with lists, Enumerations,
;;; Iterators, Iterables, and Arrays.
(define better-for-each
  (case-lambda
   (((f :: procedure) (l :: list))
    (for-each f l))
   (((f :: procedure) (l :: java.util.Enumeration))
    (for-each-enumeration f l))
   (((f :: procedure) (l :: java.util.Iterator))
    (for-each-iterator f l))
   (((f :: procedure) (l :: java.lang.Iterable))
    (for-each-iterable f l))
   (((f :: procedure) (l :: Object))
    (for-each-array f l))
   (((f :: procedure) (l1 :: Object) (l2 :: Object) . rest)
    (apply for-each f (append (map ->list `(,l1 ,l2))
                              (map ->list rest))))))
