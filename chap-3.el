;;; 3-1

(defun power (x k)
  "Returns X to the power K."
  (cond
   ((= k 0) 1)
   ((= k 1) x)
   ((= (% k 2) 0) (* (power x (/ k 2)) (power x (/ k 2))))
   (t (* x (power x (/ k 2)) (power x (/ k 2))))))

;;; 3-2

(defun cannonball (n)
  "Returns the sum of the first N squares."
  (cond
   ((<= n 1) 1)
   (t (+ (* n n) (cannonball (1- n))))))

;;; 3-3

(defun square (n)
  "Returns the value of N squared by summing the first N odd integers."
  (cond
   ((= n 1) 1)
   (t (+ (- (* 2 n) 1) (square (1- n))))))

;;; 3-4

(defun gcd (x y)
  "Returns the greatest common divisor of X and Y.
Implements the Euclidean algorithm."
  (cond
   ((< x y) (gcd y x))
   ((= (% x y) 0) y)
   (t (gcd y (% x y)))))

;;; 3-5

(defun p (n k)
  "Returns the number of permutations of N objects taken K at a time."
  (cond
   ((<= k 1) n)
   (t (* n (p (1- n) (1- k))))))

;;; 3-6

(defun c (n k)
  "Returns the number of combinations of N objects taken K at a time.
Uses relationships in Pascal's triangle."
  (cond
   ((or (= n 0) (= k 0) (= n k)) 1)
   (t (+ (c (1- n) (1- k)) (c (1- n) k)))))