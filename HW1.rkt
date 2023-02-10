#lang racket
; Paige Biggs pmb66

;inorder?
;checks base cases of empty list or single element in list => true
;recursively check if the first element of the list is less than or equal to the second element
;stop and return false if the first element is not >= the second element
;stop when there is one element left in the list

(define inorder?
  (lambda (lis)
    (cond
      [(null? lis) #t]
      [(null? (cdr lis)) #t]
      [(or (< (car lis) (car (cdr lis))) (= (car lis) (car (cdr lis))))
       (inorder? (cdr lis))]
      [else #f])))


;dotproduct
;adds zero and stops recursion when either of the lists become empty
;adds the product of the first two numbers of the lists
;recurses on next elements of the lists 

(define dotproduct
  (lambda (lisA lisB)
    (if (or (null? lisA) (null? lisB))
        0
        (+
         (* (car lisA) (car lisB)) (dotproduct (cdr lisA) (cdr lisB))))))


;squareroot
;base case of iteration = 0
;while iteration > 0, recursive, decreasing iteration each time
;recrusion takes place in old as (squareroot value (-itr 1)) , value stays constant

(define squareroot 
  (lambda (value itr)
    (cond
      [(zero? itr) value]
      [else
       (- (squareroot value (- itr 1)) (/ (- (* (squareroot value (- itr 1)) (squareroot  value (- itr 1))) value) (* 2 (squareroot value (- itr 1)))))]))) 
 

;removesubsequence
;base case is lisA being null, stop searching for duplicates if the list is empty
;also stop searching if at the end of lisB
;keep cons lisB over until finding a match from lisA, then just skip that element when cons and look for the next element from lisA

(define removesubsequence
  (lambda (lisA lisB)
    (cond
      [(null? lisA) lisB]
      [(null? (cdr lisB)) '()]
      [(eq? (car lisA) (car lisB))
       (cons (car (cdr lisB)) (removesubsequence (cdr lisA) (cdr (cdr lisB))))]
      [else (cons (car lisB) (removesubsequence lisA (cdr lisB)))])))

  
;reverse* takes a nested list and reverses the contents of the list and all nested lists
;checks if list is empty
;create new list with cons and recursively append to the end to reverse and preserve paratheses order

(define reverse*
  (lambda (lis)
    (cond
      [(null? lis)'()]
      [(pair? (car lis))
       (append (reverse* (cdr lis)) (cons (reverse* (car lis)) '()))]
      [else (append (reverse* (cdr lis)) (cons (car lis) '()))])))


;first* takes a list of lists and returns the first (left most) atom that appears in the list
;checks for empty list
;recurses until getting to the first non pair (element) and returns the element

(define first*
  (lambda (lis)
    (cond
      [(null? lis) '()]
      [(pair? (car lis)) (first* (car lis))]
      [(not (pair? (car lis))) (car lis)]
      [else (first* (cdr lis))])))


;last*
;similar to first* expect with the additional null? check
;recurses until getting to the last element (checked by (null? (cdr lis)))

(define last*
  (lambda (lis)
  (cond
    [(null? lis) '()]
    [(and (null? (cdr lis)) (pair? (car lis))) ;get to last sublist
     (last* (car lis))]
    [(and (null? (cdr lis)) (not (pair? (car lis)))) ;get last element in last sublist
     (car lis)]
    [else (last* (cdr lis))])))

        
;helper method based on inorder?
;is inorder if just an element/not a list 
;is inorder if empty list or single element in list
;check for lists to recurse on sublists
;at any point stop if a previous element is greater than a consequtive one

(define checkorder
  (lambda (lis)
    (cond
      [(not (list? lis)) #t]
      [(null? lis) #t]
      [(null? (cdr lis)) #t]
      [(list? (car lis)) (checkorder (car lis))]
      [(list? (car (cdr lis))) (checkorder (cdr lis))]
      [(> (car lis) (car (cdr lis))) #f]
      [else (checkorder (cdr lis))])))


;helper method from class
;sums sublists
;to be called on each sublist to compare summations in numorder*?

(define sumnumbers*
  (lambda (lis)
    (cond
      [(null? lis) 0]
      ((not (list? lis)) lis)
      [(number? (car lis)) (+ (car lis) (sumnumbers* (cdr lis)))]
      [(pair? (car lis)) (+ (sumnumbers* (car lis)) (sumnumbers* (cdr lis)))]
      [else (+ 0 (sumnumbers* (cdr lis)))])))


;numorder*?
;takes a nested list of numbers, returns #t if values of entries in the list and all sublists are in non-decreasing order.
;checks for empty or single element lists
;uses checkorder helper method to determine if (car lis) + sublists are in order
;uses sumnumbers* to get the summation value for each sublist and compares it to the next sublist
;recurse through each sublist, if at any point one of the statements returns false, numorder*? returns false
;otherswise returns true when all sublists have been checked

(define numorder*?
  (lambda (lis)
    (cond
      [(or (null? lis) (null? (cdr lis))) #t] 
      [(not (checkorder (car lis))) #f]
      [(>= (sumnumbers* (car lis)) (sumnumbers* (car (cdr lis)))) #f]
      [else (numorder*? (cdr lis))])))
      

;implemenation of map function from class
;helper method for vectormult
;applies a function to each element in list

(define mymap
  (lambda (f lis)
    (if (null? lis)
        '()
        (cons (f (car lis)) (mymap f (cdr lis))))))


;helper method similar to mymap used to transpose matrix
;takes in a matrix and switches columns and rows
;applies car function to values using mymap to get first elements
;'((0 2 3) (1 2 0) (1 0 3))) => '((0 1 1) (2 2 0) (3 0 3))

(define transpose 
  (lambda (lis)
    (if (null? (car lis))
     '()
      (cons (mymap car lis) (transpose (mymap cdr lis))))))



;vectormult
;takes a row vector (a list of numbers) and matrix (a list of lists of numbers) and multiplies the vector times the matrix.
;use mymap helper function to apply dotproduct function to every column in matrix
;lambda (col) creates temp column to pull from matrix to act as another vector
;use transpose function on matrix so that mymap will pull columns instead of rows

(define vectormult
  (lambda (vec mat)
    (if (null? vec)
        '()
        (mymap (lambda (col) (dotproduct vec col)) (transpose mat)))))
     

;matrixmultiply
;takes two matrices (a list of lists of numbers) and multiplies them
;similar to vectormult, calls vectormult instead of dotproduct and mymap applies recursively
;no need to transpose mat twice since it is already done in vectormult

(define matrixmultiply
(lambda (mat1 mat2)
  (if (null? mat1)
        '()
        (mymap (lambda (row) (vectormult row mat2)) mat1))))

