(defpackage :genetic-algorithm
  (:use :cl :numcl))

(in-package :genetic-algorithm)

(defun fitness-function (x1 x2)
  (- (sin x1) (cos x2) (tan (+ x1 x2)) (* 3/4 (exp (- 1 (sqrt (expt x1 2)))))))

(defun initialize-population (size)
  (loop for i from 1 to size collect (list (+ -10 (random 20)) (+ -10 (random 20)))))

(defun decode-chromosome (chromosome)
  (values (first chromosome) (second chromosome)))

(defun calculate-fitness (population)
  (mapcar (lambda (ind) (fitness-function (first ind) (second ind))) population))

(defun select-parents (population fitness)
  (let* ((total-fitness (reduce #'+ fitness))
         (probabilities (mapcar (lambda (f) (/ f total-fitness)) fitness))
         (selected (loop for i from 0 below 2 collect (random (length population) probabilities))))
    (list (nth (first selected) population) (nth (second selected) population))))

(defun mutate (chromosome &optional (mutation-rate 0.1))
  (when (< (random 1.0) mutation-rate)
    (setf (first chromosome) (+ (first chromosome) (random-normal 0 1))
          (second chromosome) (+ (second chromosome) (random-normal 0 1))))
  (mapcar (lambda (x) (min 10 (max -10 x))) chromosome))

(defun generative-algorithm (pop-size generations)
  (let ((population (initialize-population pop-size))
        (best-chromosome nil)
        (best-fitness most-positive-fixnum))
    (dotimes (gen generations)
      (let ((fitness (calculate-fitness population)))
        (let ((current-best-idx (position (reduce #'max fitness) fitness)))
          (when (> (nth current-best-idx fitness) best-fitness)
            (setf best-fitness (nth current-best-idx fitness)
                  best-chromosome (nth current-best-idx population))))
        (let ((new-population nil))
          (dotimes (i (/ pop-size 2))
            (let ((parents (select-parents population fitness)))
              (let ((offspring1 (mutate (copy-list (first parents))))
                    (offspring2 (mutate (copy-list (second parents)))))
                (push offspring1 new-population)
                (push offspring2 new-population))))
          (setf population (nreverse new-population)))))
    (multiple-value-bind (x1 x2) (decode-chromosome best-chromosome)
      (values best-chromosome x1 x2)))

;; Example usage
(multiple-value-bind (best-chromosome x1 x2) (generative-algorithm 100 1000)
  (format t "Best Chromosome: ~a, x1: ~a, x2: ~a" best-chromosome x1 x2))