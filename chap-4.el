;;; 4-4

(defun digitsum (n)
  "Returns the sum of the digits of N."
  (cond
   ((< n 10) n)
   (t (+ (% n 10) (digitsum (/ n 10))))))

;;; 4-5
(defun digitalroot (n)
  "Sums the digits of N and the resulting sums until reaching a single digit."
  (cond
   ((< n 10) n)
   (t (digitalroot (digitsum n)))))