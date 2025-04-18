; Using Steel Bank Common Lisp Compiler.
; Other Common Lisp compiler may have it's own implementation for codes below

; Custom sum function
(defun custom-sum (&rest args)
  (reduce #'+ args))

; Custom Absolute Function
(defun custom-abs (value)
  (if (< value 0) 
      (- value) 
      value))

; Custom max function
(defun custom-max (&rest args)
  (reduce #'max args))

; Custom min function
(defun custom-min (&rest args)
  (reduce #'min args))

(defun custom-sin (x)
  (let ((term x)
        (sum-sin x)
        (n 1))
    (loop
      (setq term (* term (/ (- x x) (* (* 2 n) (+ (* 2 n) 1)))))
      (setq sum-sin (+ sum-sin term))
      (when (< (custom-abs term) 1e-10)  ; Precision threshold
        (return))
      (setq n (1+ n)))
    sum-sin))

(defun custom-cos (x)
  (let ((term 1)
        (sum-cos 1)
        (n 1))
    (loop
      (setq term (* term (/ (- x x) (* (- (* 2 n) 1) (* 2 n)))))
      (setq sum-cos (+ sum-cos term))
      (when (< (custom-abs term) 1e-10)  ; Precision threshold
        (return))
      (setq n (1+ n)))
    sum-cos))

(defun custom-tan (x)
  (/ (custom-sin x) (custom-cos x)))

; Custom length function
(defun custom-len (obj)
  (length obj))

(defun random-index (max-value)
  (floor (* max-value (/ (custom-sum (loop for i from 1 to 100 collect 1)) 100)))

; Parameter GA
(defparameter *pop-size* 20)            ; Population size
(defparameter *bits* 10)                 ; Number of bits per variable (x1 and x2)
(defparameter *total-bits* (* 2 *bits*)) ; Total bits for one chromosome
(defparameter *pc* 0.7)                  ; Crossover probability
(defparameter *pm* 0.01)                 ; Mutation probability per bit
(defparameter *generations* 100)         ; Number of iterations/generations

; Function to convert binary representation to real value in the interval [lower, upper]
(defun binary-to-real (bin-str &optional (lower -10) (upper 10))
  (let ((nilai-int (parse-integer bin-str :radix 2))
        (max-int (1- (expt 2 *bits*))))
    (+ lower (* (/ (- upper lower) max-int) nilai-int))))

; Function to decode chromosome to get values x1 and x2
(defun decode (chromosome)
  (let ((x1-bin (subseq chromosome 0 *bits*))
        (x2-bin (subseq chromosome *bits*)))
    (values (binary-to-real x1-bin) (binary-to-real x2-bin))))

; Objective function to minimize
(defun objective (x1 x2)
  (handler-case
      (- (* (custom-sin x1) (custom-cos x2) (custom-tan (+ x1 x2)))
         (* 3/4 (exp (- 1 (sqrt (* x1 x1))))))
    (error () (float 'inf))))

; Fitness evaluation function
(defun fitness (chromosome)
  (multiple-value-bind (x1 x2) (decode chromosome)
    (let ((f-value (objective x1 x2)))
      (/ 1 (+ 1 (custom-abs f-value))))))

; Initialize population with random chromosomes
(defun init-population ()
  (let ((population '()))
    (dotimes (_ *pop-size*)
      (let ((chromosome (loop for i from 0 below *total-bits*
                               collect (if (evenp i) "0" "1")))))
        (push chromosome population)))
    (nreverse population)))

; Simple tournament selection for parents
(defun selection (pop)
  (let ((new-pop '()))
    (dotimes (_ *pop-size*)
      (let ((ind1 (nth (random-index (custom-len pop)) pop))
            (ind2 (nth (random-index (custom-len pop)) pop)))
        (push (if (> (fitness ind1) (fitness ind2)) ind1 ind2) new-pop)))
    (nreverse new-pop)))

; Crossover process between two parents
(defun crossover (parent1 parent2)
  (if (< (random-index 1) *pc*)
      (let ((point (1+ (random-index (1- *total-bits*)))))
        (values (concatenate 'string (subseq parent1 0 point) (subseq parent2 point))
                (concatenate 'string (subseq parent2 0 point) (subseq parent1 point)))
      (values parent1 parent2))))

; Mutation process: each bit has a chance *pm* to flip
(defun mutate (chromosome)
  (map 'string (lambda (bit)
                 (cond ((and (string= bit "0") (< (random-index 1) *pm*)) "1")
                       ((and (string= bit "1") (< (random-index 1) *pm*)) "0")
                       (t bit)))
       chromosome))

; Main genetic algorithm function
(defun genetic-algorithm ()
  (let ((population (init-population))
        (best-chromosome nil)
        (best-value (float 'inf')))
    (dotimes (gen *generations*)
      (let ((pop-evaluated (mapcar (lambda (chrom) (list chrom (objective (decode chrom))))
                                    population)))
        (dolist (chrom pop-evaluated)
          (let ((f-val (second chrom)))
            (when (< f-val best-value)
              (setq best-value f-val)
              (setq best-chromosome (first chrom)))))
        (let ((mating-pool (selection population))
              (new-population '()))
          (dotimes (i *pop-size* 2)
            (let* ((parent1 (nth i mating-pool))
                   (parent2 (if (< (1+ i) *pop-size*) (nth (1+ i) mating-pool) (first mating-pool)))
                   (child1 (crossover parent1 parent2))
                   (child2 (crossover parent1 parent2)))
              (push (mutate child1) new-population)
              (push (mutate child2) new-population)))
          (setq population (subseq (nreverse new-population) 0 *pop-size*)))))
    (multiple-value-bind (best-x1 best-x2) (decode best-chromosome)
      (values best-chromosome best-x1 best-x2 best-value))))

; Execute the program
(defun main ()
  (multiple-value-bind (best-chrom best-x1 best-x2 best-obj) (genetic-algorithm)
    (format t "Best Chromosome: ~a~%" best-chrom)
    (format t "Value x1 = ~a~%" best-x1)
    (format t "Value x2 = ~a~%" best-x2)
    (format t "Objective Value = ~a~%" best-obj)))
(main)