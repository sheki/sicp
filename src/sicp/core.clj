(ns sicp.core
  (:gen-class))

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
      (* 3 (dosome (- n 3)))
      )))

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
