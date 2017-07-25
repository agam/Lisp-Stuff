;;; Some stuff that helps with the "Prime number" questions at Project Euler
;; 
;;; Depends on `iterate`, which can be installed via `(ql:quickload 'iterate)`
;;
;;; Use as follows:
;;
;; (initialize-primes)
;; (mark-primes)
;;
;;; Then, inspect *primes* as necessary. A couple of helper functions:
;;
;; (inspect-num-range 1000 1200)     ; Shows the "primeness" status of numbers between 1000 and 1200
;; (inspect-num-list 45 78 92 2554)  ; Shows the "primeness" status of numbers passed in
;; 
;;; If any use of `nth-primes` is needed, run
;;
;; (precalculate-nth-primes)
;;
;;; Then, inspect *nth-primes* as necessary.
;;
;; Tested on Lispworks and SBCL, no operation should take more than some fraction of a second.
;;

(defpackage euler
  (:use :cl :iterate))

(in-package :euler)

(defparameter *max-primes* (* 10 1000 1000) 
  "Maximum number of primes to compute and store _before_ doing anything")

(defparameter *nth-primes* nil
  "Indexed list of the nth primes, beginning at 2 as the 0th prime")

(defparameter *primes* nil
  "List of numbers, with primes marked as 't'")

(defun initialize-primes ()
  (setf *primes* (make-array '(#.*max-primes*)
                             :element-type 'boolean
                             :initial-element t))
  nil) ;; Don't return anything, least of all the massive array we just built!


(defun mark-multiples (n limit)
  (iter 
   (for i from (* 2 n) below limit by n)
   (setf (aref *primes* i) nil)))

(defun find-next-candidate (prime limit)
  (iter
   (for i from prime below limit)
   (finding i such-that (eq (aref *primes* i) t))))

;;; Timing notes:
;; 10M -> 0.054

(defun mark-primes (&optional (limit *max-primes*))
  (setf (aref *primes* 0) nil)
  (setf (aref *primes* 1) nil)
  (iter
   (for prime first 2 then (find-next-candidate (1+ prime) limit))
   (until (null prime))
   (mark-multiples prime limit)))

(defun inspect-num-range (start end)
  (iter
   (for i from start below end)
   (format t "~A -> ~A~%" i (aref *primes* i))))

(defun inspect-num-list (&rest list)
  (iter
   (for i in list)
   (format t "~A -> ~A~%" i (aref *primes* i))))

(defun is-prime (n)
  (eq (aref *primes* n) t))

(defun precalculate-nth-primes ()
  (setf *nth-primes* (make-array `(,(/ *max-primes* 10)) :element-type 'fixnum))
  (iter
    (for p from 2 below *max-primes*)
    (initially (setq i 0))
    (for i next
         (if (is-prime p)
             (progn
               (setf (aref *nth-primes* i) p)
               (incf i))
           i))))

(defun nth-prime (n)
  (aref *nth-primes* n))
