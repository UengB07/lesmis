;;;Some Exercises of Touretskys 

;;; Just a preview of LISP review material if I ever need it. JS and Haskell will
;;; be used now instead of LISP.

(defun add (x y) (+ x y))
(defun subtract (x y) (- x y))
(defun multiply (x y) (* x y))
(defun divide (x y) (/ x y))

(defun longer-than (x y) 
  (> (length x) (length y)))

(defun addlength (x) (cons (length x) x))

(defun call-up (caller callee)
  (list 'hello callee 'this 'is
    caller 'calling))

(defun swap (x)
  (list (second x) (first x)))

(defun make-even (x)
  (if (oddp x) (+ x 1) x))

(defun further (x)
  (if (> x 0) (+ x 1) (- x 1)))

(defun my-not (x)
  (if x nil t))

(defun ordered (x y)
  (if (> y x) (list x y) (list y x)))

(defun my-abs (x)
  (cond ((> x 0) x)
        ((< x 0) (- 0 x))
        (t 0)))

(defun make-odd (x)
  (cond ((not (oddp x)) (+ x 1))
        (t x)))

(defun constrain (x min max) ;; Good Function
  (cond ((< x min) min)
        ((> x max) max)
        (t x)))

(defun cycle (x)  ;; The Basis of For Loops
  (cond ((< x 99) (cycle (print (+ 2 x))))
        (t x)))

(defun geq (x y)
  (or (> x y)
      (equal x y)))

(defun fst (x)
  (cond ((and (oddp x) (> x 0)) (* x x))
        ((and (oddp x) (< x 0)) (* x 2))
        (t (/ x 2))))


(defun genderish (x y)
  (cond ((and (or (equal x 'boy) (equal x 'girl)) (equal y 'child)))
        ((and (or (equal x 'man) (equal x 'woman)) (equal y 'adult)))
        (t 'none)))

(defun good-style (p) ;;; LET
  (let ((byeight (+ p 5)))
    (list 'result 'is byeight)))

(defun numLegs (x y z)
  "Practice of using let and let*."
  (let* ((sum (+ x y z))
        (product (* x y z))
        (average (/ sum 3))
        (difference (- x y z)))
  (list 'Sum 'is sum  'product 'is product 'difference 'is difference 'avg average)))

(defun throw-die ()
  (let ((toss (+ 1 (random 6))))
  (list toss)))

(defun throw-dice ()
  (let* ((die1 (+ 1 (random 6)))
        (die2 (+ 1 (random 6)))
        (diesum (+ die1 die2)))

      (cond ((and (equal die1 6) (equal die2 6)) (list 'BOXCAR))
        ((and (equal die1 1) (equal die2 1)) (list 'SNAKEEYES))
        (t (print (list die1 die2))))
      
      (cond ((equal diesum 7) (print 'WIN))
            ((equal diesum 11) (print 'WIN))
            ((equal diesum 2) (print 'LOSE))
            ((equal diesum 3) (print 'LOSE))
            ((equal diesum 12) (print 'LOSE))
            (t (print 'GO-ON)))

      (list 'SUM 'IS diesum)
))

;;; TABLE TEMPLATE
(defun tablepractice ()

(let ((words
  '((one un)
    (two dos)
    (three tres)
    (four cuatro)
    (five cinco))
  ))

    (write words)
    (assoc 'three words)

 )
)
;;;

(defun who-wrote (x)

(let ((books
  '((war-and-peace leo-tolstoy)
    (brothers-karamakazov fyodor-dostovesky)
    (heart-of-darkness joseph-conrad)
    (les-miserables victor-hugo)
    (alices-adventures-in-wonderland lewis-caroll))
  ))

    (rest (assoc x books))
    ;;; (who-wrote 'war-and-peace)
 )
)


(defun nerdus (x)

(let ((nerd-states
  '((sleeping eating)
    (eating waiting-for-a-computer)
    (waiting-for-a-computer programming)
    (programming debugging)
    (debugging sleeping))
  ))

    (rest (assoc x nerd-states))
    
 )
)

;;;sleepless-nerd just do not put first ,replaace last sleepign to eating...

;;;EXTRACT DATA FROM TABLE
(defun getans (x)
   (nth 2 x))

(setf daily-planet

  '((olsen jimmy 123-76-4535 cub-reporter)
    (kent clark 089-52-6787 reporter)
    (lane lois 951-26-1438 reporter)
    (white perry 355-16-7439 editor))

)

(defun ex2 ()
  (mapcar #'getans daily-planet))
  ;;;getting done quick; consider --> (mapcar #'(lambda (x) (nth 2 x)) daily-planet)
;;;EXTRACT DATA FROM TABLE

;;;;EX3
(defun ud (x)
(cond ((equal x 'up) 'down)
      ((equal x 'down) 'up)))

(defun ex3 ()
  (mapcar #'ud '(up down up up))
)

;;;ALT

(defun ex3 ()
  (mapcar #'(lambda (x) (cond ((equal x 'up) 'down) ((equal x 'down) 'up))) '(up down up up))
)

;;;;EX3


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setf note-table

  '((c . 1)
    (c-sharp . 2)
    (d . 3)
    (d-sharp . 4)
    (e . 5)
    (f . 6)
    (f-sharp . 7)
    (g . 8)
    (g-sharp . 9)
    (a . 10)
    (a-sharp . 11)
    (b . 12))

)

;;;;b)

(defun translate (x) 
  (cdr (assoc x note-table))
) 

(defun numbers (v)
  (mapcar #'translate v)
)

;;;;c)

(defun rvtranslate (x)
  (car (rassoc x note-table))
)

(defun notes (f)
  (mapcar #'rvtranslate f)
)

;;;!!!!e)
(defun raise (n y)
  (mapcar #'(lambda (x) (+ x n)) y)
)

(defun normalize (x)
  (if (> x 12) (- x 12) x)
)

;;;f)

(defun mapnormalize (y)
  (mapcar #'normalize y)
)
;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
