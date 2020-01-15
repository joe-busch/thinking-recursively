(defun hanoi (n start finish temp)
  "Solves Tower of Hanoi problem by moving N disks from START to FINISH.
Uses TEMP as temporary pin. Steps appear in the *Messages* buffer."
  (cond
   ((= n 1) (message "move %s to %s" start finish))
   (t
    (progn
      (hanoi (1- n) start temp finish)
      (hanoi 1 start finish temp)
      (hanoi (1- n) temp finish start)))))
