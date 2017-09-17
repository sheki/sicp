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


