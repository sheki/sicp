(ns sicp.core
  (:require [clojure.math.numeric-tower :as math]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(if true
  "By Zeus's hammer!"
  "By Aquaman's trident!")

(if false
  "By Odins Elbow")

(def failed-protagonist-names
  ["Larry Potter" "Doreen the Explorer" "The Incredible Bulk"])

failed-protagonist-names

; 1.3

(defn square
  "square of a number"
  [a]
  (* a a))

(defn square_sum
  "sum of square of 2 numbers"
  [a b]
  (+ (square a) (square b)))

(defn square_sum_equal
  "sum squares if equal"
  [a b]
  (if (> a b) (square_sum a a)
      (square_sum a b)))

(defn largest_square_sum
  "takes 3 numbers and returns the square of the largest 3"
  [a b c]
  (cond
    (= a b) (square_sum_equal a c)
    (= a c) (square_sum_equal a b)
    (= b c) (square_sum_equal b a)
    (and (> a b) (> a c) (> b  c)) (square_sum a b)
    (and (> a b) (> a c) (> c  b)) (square_sum a c)
    (and (> b a) (> b c) (> a c)) (square_sum a b)
    (and (> b a) (> b c) (> c a)) (square_sum b c)
    (and (> c a) (> c b) (> b a)) (square_sum b c)
    (and (> c a) (> c b) (> a b)) (square_sum a c)))

(defn abs
  [x]
  (if (< x 0)
    (* x -1)
    x))

(defn avg
  [x y]
  (/ (+ x y) 2))

(defn square
  [x]
  (* x x))

(defn improve
  [guess x]
  (avg guess (/ x guess)))

(defn good-enough?
  [old newg]
  (< (/ (abs (- old newg)) old) 0.00001))

(defn sqrt-iter
  [guess x]
  (if (good-enough? guess (improve guess x))
    guess
    (sqrt-iter (improve guess x)
               x)))

(defn improvecbrt
  [y x]
  (/ (+ (/ x (square y)) (* 2 y)) 3))

(defn cbrt-iter
  [guess x]
  (if (good-enough? guess (improvecbrt guess x))
    guess
    (cbrt-iter (improvecbrt guess x)
               x)))

(defn sqrt
  [x]
  (sqrt-iter  1.0 x))

(defn cbrt
  [x]
  (cbrt-iter 1.0 x))

;; 1.11 

(defn dosome
  [n]
  (if (< n 3) n
      (+
       (dosome (- n 1))
       (* 2 (dosome (- n 2)))
       (* 3 (dosome (- n 3))))))

(defn dosome-iter
  [n1 n2 n3 count n]
  (if (= count n) (+ n1 (* 2 n2) (* 3 n3))
      (dosome-iter (+ n1 (* 2 n2) (* 3 n3)) n1 n2 (+ 1 count) n)))

(defn dosome2
  [n]
  (if (< n 3) n
      (dosome-iter 2 1 0 3 n)))

(defn pascal
  [row col]
  (cond (= col 1) 1
        (= col row) 1
        :else (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1)))))

(defn sum
  [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn cube
  [x]
  (* x x x))

(defn integral
  [f a b dx]
  (defn add-dx [x] (+ x dx))
  (* (sum f (+ a  (/ dx 2.0)) add-dx b)
     dx))

(defn simpson
  [f a b n]
  (def h (/ (- b a) n))
  (defn y [k]
    (f (+ a (* k h))))
  (defn term [x]
    (cond (odd? x) (* 4 (y x))
          (or (= x 0) (= x n)) (y x)
          :else (* 2 (y x))))
  (/ (* h (sum term 0 inc n)) 3))

(defn sumiter
  [term a next b]
  (defn iter [a result]
    (if (= a b)
      (+ result (term a))
      (iter (next a) (+ result (term a)))))
  (iter a 0))

; 1.30
(defn simpsoniter
  [f a b n]
  (def h (/ (- b a) n))
  (defn y [k]
    (f (+ a (* k h))))
  (defn term [x]
    (cond (odd? x) (* 4 (y x))
          (or (= x 0) (= x n)) (y x)
          :else (* 2 (y x))))
  (/ (* h (sumiter term 0 inc n)) 3))

; 1.31
(defn product
  [term a next b]
  (if (= a b)
    b
    (* (term a)
       (product term (next a) next b))))

(defn factorial
  [n]
  (product identity 1 inc n))

(defn productiter
  [term a next b]
  (defn iter [a result]
    (if (= a b)
      (* result (term a))
      (iter (next a) (* result (term a)))))
  (iter a 1))

(defn factorialiter
  [n]
  (productiter identity 1 inc n))

(defn pi4
  [n]
  (defn num [x]
    (cond
      (= x 0) 2
      (odd? x) (+ 2 (num (dec x)))
      :else (num (dec x))))

  (defn denom [x]
    (cond
      (= x 0) 3
      (even? x) (+ 2 (denom (dec x)))
      :else (denom (dec x))))
  (/ (product num 0 inc n)
     (product denom 0 inc n)))

; 1.32

(defn accumulate
  [combiner null-value term a next b]
  (defn iter [a result]
    (if (= a b)
      (combiner result (term a))
      (iter (next a) (* result (term a)))))
  (iter a null-value))

(defn factorial2
  [n]
  (accumulate * 1 identity 1 inc n))

; 1.32 accumulate recursive
(defn accumrecursive
  [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a)
              (accumrecursive combiner null-value term (next a) next b))))

(defn factorial3
  [n]
  (accumrecursive * 1 identity 1 inc n))

(defn filtered-accumulate
  [combiner null-value term a next b fltr]
  (if (> a b)
    null-value
    (if (fltr (term a))
      (combiner (term a)
                (accumrecursive combiner null-value term (next a) next b))
      (accumrecursive combiner null-value term (next a) next b))))

(defn f
  [g]
  (g 2))

(defn close-enough? [x y]
  (< (abs (- x y)) 0.001))

(defn positive? [x] (> x 0))
(defn negative? [x] (< x 0))

(defn search
  [f neg-point pos-point]
  (let [midpoint (avg neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
      midpoint
      (let
       [test-value (f midpoint)]
        (cond
          (positive? test-value) (search f neg-point midpoint)
          (negative? test-value) (search f midpoint pos-point)
          :else midpoint)))))

(defn half-interval-method
  [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond
      (and (negative? a-value) (positive? b-value)) (search f a b)
      (and (negative? b-value) (positive? a-value)) (search f b a)
      :else "Values are not of opposite sign")))

(def tolerance 0.00001)
(defn fixed-point [f, first-guess]
  (defn close-enough? [v1 v2]
    (< (abs (- v1 v2)) tolerance))
  (defn tryy [guess]
    (let [next (f guess)]
      (if (close-enough? guess next)
        next
        (tryy next))))
  (tryy first-guess))

(defn log [x] (Math/log x))
;(fixed-point (fn [x] (/ (log 1000) (log x))) 111)

; 1.41
(defn double
  [f]
  (fn [x] (f (f x))))

;1.42
(defn compose
  [f g]
  (fn [x] (f (g x))))

;1.43
(defn repeated-helper
  [f ag n]
  (if (= n 1) ag (repeated-helper f (compose f ag) (- n 1))))
(defn repeated
  [f n]
  (repeated-helper f f n))

;1.44 
(defn smooth
  [f dx]
  (fn [x] (/ (+ (f (- x dx)) (f x) (f (+ x dx))))))

(defn nsmooth
  [f dx n]
  (repeated (smooth dx) n))

; 2.1
(defn pair
  [a b]
  [a b])

(defn make-rat
  [n d]
  (cond
    (and (> n 0) (> d 0)) (pair n d)
    (and (> n 0) (< d 0)) (pair (* -1 n) (* -1 d))
    (and (< n 0) (< d 0)) (pair (* -1 n) (* -1 d))
    (and (< n 0) (> d 0)) (pair n d)))

(defn numer [rat] (first rat))
(defn denom [rat] (second rat))

(defn print-rat
  [x]
  (print (numer x))
  (print "/")
  (print (denom x)))

;2.1
(defn make-point
  [x y]
  (pair x y))

(defn x-point [p] (first p))
(defn y-point [p] (second p))

(defn make-segment
  [p1 p2]
  (pair (apply make-point p1) (apply make-point p2)))

(defn midpoint-segment
  [s]
  (make-point
   (/ (+ (x-point (first s)) (x-point (second s))) 2)
   (/ (+ (y-point (first s)) (y-point (second s))) 2)))

(defn print-point
  [p]
  (print "(")
  (print (x-point p))
  (print ",")
  (print (y-point p))
  (print ")"))

;2.3
(defn make-rectangle
  [p1 p2 p3 p4]
  [(pair p1 p2) (pair p3 p4)])

(defn rect-p1
  [rect] (first (first rect)))

(defn rect-p2
  [rect] (second (first rect)))

(defn rect-p3
  [rect] (first (second rect)))

(defn rect-p4
  [rect] (second (second rect)))

(defn segment-length
  [p1 p2]
  (sqrt
   (+
    (square (- (x-point p1) (x-point p2)))
    (square (- (y-point p1) (y-point p2))))))

(defn area-rect
  [rect]
  (*
   (segment-length (rect-p1 rect) (rect-p2 rect))
   (segment-length (rect-p2 rect) (rect-p3 rect))))

(defn peri-rect
  [rect]
  (* 2 (+
        (segment-length (rect-p1 rect) (rect-p2 rect))
        (segment-length (rect-p2 rect) (rect-p3 rect)))))

(area-rect (make-rectangle (make-point 0 0) (make-point 3 0) (make-point 3,5) (make-point 0 5)))

;2.4
(defn cons_mine
  [x y]
  (fn [m] (m x y)))
(defn car
  [z]
  (z (fn [p q] p)))
(defn cdr
  [z]
  (z (fn [p q] q)))

;2.5
(defn pow [x y] (if (= y 0) 1
                    (* x (pow x (- y 1)))))

(defn cons_p [x y]
  (* (pow 2 x) (pow 3 x)))

(defn car_p [x]
  (if (= (mod x 2) 0) (+ 1 (car_p (/ x 2))) 0))

(defn cdr_p [x]
  (if (= (mod x 3) 0) (+ 1 (car_p (/ x 3)))
      0))

;2.6

(defn zero []
  (fn [f] (fn [x] x)))

(defn one []
  (fn [f] (fn [x] (f x))))

(defn two []
  (fn [f] (fn [x] (f (f x)))))

(defn add-1 [n]
  (fn [f] (fn [x] ((n f) x))))

(defn add [m n]
  (fn [f] (fn [x] ((n f) ((m f) x)))))

(defn make-interval [x y] (cons_mine x y))
(defn upper-bound [x] (cdr x))
(defn lower-bound [x] (car x))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; 2.12
(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))
(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn make-center-percent [c p]
  (make-interval (- c (* (/ p 100.0) c) (+ c (* (/ p 100.0) c)))))

(defn percent [i]
  (* 100.0 (/ (width i)  (center i))))

(defn list-ref [items n]
  (if (= n 0)
    (first items)
    (list-ref (rest items) (- n 1))))

(defn length  [items]
  (if (empty? items)
    0
    (+ 1 (length (rest items)))))

(defn length-iter [items]
  (defn length-iter [a count]
    (if (empty? a)
      count
      (length-iter (rest a) (+ 1 count))))
  (length-iter items 0))

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

; 2.17
(defn last-pair [l]
  (if (= (length (rest l)) 1) (rest l)
      (last-pair (rest l))))

; 2.18

(defn reverse [inp]
  (defn rev-helper [list1 list2]
    (if (empty? list1)
      list2
      (rev-helper (rest list1) (cons (first list1) list2))))
  (rev-helper inp (list)))

;2.20
(defn same-parity [x & y]
  (if (empty? y) true
      (if (= (even? x)  (even? (first y))) (apply same-parity x (rest y))
          false)))

(defn my-map [proc items]
  (if (empty? items)
    nil
    (cons (proc (first items))
          (map proc (rest items)))))

(defn square-list [items]
  (map  square items))

;2.23
(defn for-each [proc items]
  (if (empty? items)
    nil
    (do
      (proc (first items))
      (for-each proc (rest items)))))

; 2.27
(defn deep-reverse [items]
  (defn rev-helper [list1 list2]
    (cond
      (empty? list1) list2
      (seq? (first list1)) (rev-helper (rest list1) (cons (deep-reverse (first list1)) list2))
      :else (rev-helper (rest list1) (cons (first list1) list2))))
  (rev-helper items (list)))

; 2.28
(def x (list (list 1 2) (list 3 (list 5 6))))
(defn fringe [items]
  (cond
    (empty? items) nil
    (seq? (first items)) (append (fringe (first items)) (fringe (rest items)))
    :else (cons (first items) (fringe (rest items)))))
;2.29
(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(defn left-branch [mobile]
  (first mobile))
(defn right-branch [mobile]
  (first (rest mobile)))
(defn branch-length [branch]
  (first branch))
(defn branch-structure [branch]
  (first (rest branch)))

(defn total-weight [mobile]
  (let [r_struc (branch-structure (right-branch mobile))
        l_struc (branch-structure (left-branch mobile))]
    (cond
      (and (seq? r_struc) (seq? l_struc)) (+ (total-weight r_struc) (total-weight l_struc))
      (seq? r_struc) (+ (total-weight r_struc) l_struc)
      (seq? l_struc) (+ (total-weight l_struc) r_struc)
      :else (+ l_struc r_struc))))

; 2.30
(defn square-tree [tree]
  (cond (nil? tree) nil
        (seq? tree) (cons (square-tree (first tree))
                          (square-tree (next tree)))
        :else (* tree tree)))

(defn square-tree [tree]
  (if (seq? tree)
    (map square-tree tree)
    (* tree tree)))

;;; Exercise 2.31

(defn tree-map [f tree]
  (if (seq? tree)
    (map f tree)
    (f tree)))

(defn square-list [tree]
  (tree-map square tree))

(defn subsets [s]
  (if (empty? s) '(())
      (let [remaining (subsets (next s))]
        (concat remaining (map (fn [x] (cons (first s) x)) remaining)))))

; 2.33
(defn new-map [p sequence]
  (reduce (fn [x y] (concat x (list (p y)))) nil sequence))

(defn new-append [seq1 seq2]
  (reduce conj  seq2 (reverse seq1)))

(defn new-length [coll]
  (reduce (fn [x y] (inc x)) 0 coll))

(defn accumulate [op init coll]         ;foldr
  (if (nil? coll) init
      (op (first coll)
          (accumulate op init (next coll)))))

(defn horner-eval [x coefficient-sequence]
  (accumulate #(+ (* %2 x) %1) 0 coefficient-sequence))

(defn accumulate-n [op init seqs]
  (if (nil? (first seqs))
    nil
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))
;;
;; (defn dot-product [v w]
;;   (accumulate + 0 (map * v w)))
;;
;; (defn matrix-*-vector [m v]
;;   (map (partial dot-product v) m))
;;
;; (defn matrix-*-matrix [m n]
;;   (let [cols (transpose n)]
;;     (map (fn [row] (map #(dot-product row %) cols)) m)))
;;
;; (defn transpose [m]
;;   (accumulate-n cons nil m))
;;
(defn equal? [l1 l2]
  (let [f1 (first l1) f2 (first l2) r1 (rest l1) r2 (rest l2)]
    (cond (and (empty? l1) (empty l2)) true
          (and (seq? f1) (seq? f2)) (and (equal? f1 f2) (equal? r1 r2))
          (and (not (seq? f1)) (not (seq? f2))) (and (= f1 f2) (equal? r1 r2))
          :else false)))

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn  =number? [exp num]
  (and (number? exp) (= exp num)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number?)) (* m1 m2)
        :else (list '* m1 m2)))
(defn sum? [x]
  (and (seq? x) (= (first x) '+)))

(defn addend [x] (first (rest x)))
(defn augend [x] (first (rest (rest x))))
(defn product? [x]
  (and (seq? x) (= (first x) '*)))
(defn multiplier [p] (first (rest p)))
(defn multiplicand [p] (first (rest (rest p))))

(defn exponentiation? [x]
  (and (seq? x) (= (first x) '**)))

(defn base [x] (= first (rest x)))
(defn exponent [x] (= first (rest (rest x))))

(defn make-exponentiation [e x]
  (cond (=number? x 0) 1
        (=number? x 1) e
        :else (list '** e x)))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp)
        (if (same-variable? exp var) 1 0)
        (sum? exp)
        (make-sum (deriv (addend exp) var)
                  (deriv (augend exp) var))
        (product? exp)
        (make-sum
         (make-product (multiplier exp)
                       (deriv (multiplicand exp) var))
         (make-product (deriv (multiplier exp) var)
                       (multiplicand exp)))
        (exponentiation? exp)
        (make-product
         (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1))
                       (deriv (base exp) var))
         :else (throw (Exception. "my exception message")))))

  ;2.59
(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (first set)) true
        :else (element-of-set? x (rest set)))) (defn intersection-set [set1 set2]
                                                 (cond (or (empty? set1) (empty? set2)) '()
                                                       (element-of-set? (first set1) set2)
                                                       (cons (first set1)
                                                             (intersection-set (rest set1) set2))
                                                       :else (intersection-set (rest set1) set2)))

(defn union-set [set1 set2]
  (cond (empty? set1) set2
        (not (element-of-set? (first set1) set2))
        (cons (first set1)
              (union-set (rest set1) set2))
        :else (union-set (rest set1) set2)))

(defn element-of-set2? [x set]
  (cond (empty set) false
        (= x (first set)) true
        (< x (rest set)) false
        :else (element-of-set? x (rest set))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

(defn union-set2 [s1 s2]
  (cond (empty? s1) s2
        (empty? s2) s1
        (= (first s1) (first s2)) (cons (first s1) (union-set2 (rest s1) (rest s2)))
        (< (first s1) (first s2)) (cons (first s1) (union-set2 (rest s1) s2))
        :else (cons (first s2) (union-set2 s1 (rest s2)))))

(defn entry [tree] (first tree))
(defn left-branch [tree] (first (rest tree)))
(defn right-branch [tree] (first (rest (rest tree))))
(defn make-tree [entry left right]
  (list entry left right))

(defn make-leaf [symbol weight]
  (list 'leaf symbol weight))

(defn symbol-leaf [x] (second x))
(defn weight-leaf [x] (nth  x 2))

(defn leaf? [object]
  (= (first object) 'leaf))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (nth tree 3)))

(defn left-branch [tree] (first tree))

(defn right-branch [tree] (nth tree 1))
(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (nth tree 2)))
(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (nth tree 3)))

(defn make-code-tree [left right]
  (list left
        right
        (conj (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn choose-branch [bit branch]
  (cond (= bit 0) (left-branch branch)
        (= bit 1) (right-branch branch)
        :else (throw "bad bit -- CHOOSE-BRANCH")))

(defn decode [bits tree]
  (defn decode-1 [bits current-branch]
    (if (empty? bits)
      '()
      (let [next-branch
            (choose-branch (first bits) current-branch)]
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (rest bits) tree))
          (decode-1 (rest bits) next-branch)))))
  (decode-1 bits tree))

(defn adjoin-set-huff [x set]
  (cond (empty? set) (list
                      (< (weight x) (weight (car set))) (cons x set)
                      :else (cons (car set)
                                  (adjoin-set-huff x (first set))))))

(defn make-leaf-set [pairs]
  (if (empty? pairs)
    '()
    (let [pair (first pairs)]
      (adjoin-set-huff (make-leaf (first pair)    ; symbol
                                  (second pair))  ; frequency
                       (make-leaf-set (rest pairs))))))

(def sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
