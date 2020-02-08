;; Mars Rover Challenge in common lisp
;; Copyright (C) 2020  Stewart V. Wright, for Vifortech Solutions
;;
;;    https://blog.vifortech.com/posts/lisp-mars-rover/
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defparameter *x_max* nil)
(defparameter *y_max* nil)

(defun rotate (move orientation)
  ;;    N
  ;;   W E
  ;;    S
  (let* ((compass '((N . (W E)) (E . (N S)) (S . (E W)) (W . (S N))))
         (possible (assoc orientation compass)))
    (if (eq move #\L)
        (cadr possible)
        (caddr possible))))


(defun plateaup (coord max)
  (when (and (>= coord 0) (<= coord max))
    t))


(defun move (pos orientation)
  (cond
    ((eq orientation 'N) (setf (cdr pos) (1+ (cdr pos))))
    ((eq orientation 'S) (setf (cdr pos) (1- (cdr pos))))
    ((eq orientation 'E) (setf (car pos) (1+ (car pos))))
    ((eq orientation 'W) (setf (car pos) (1- (car pos))))
    (t (error "Unknown orientation: ~a" orientation)))
  (unless (and (plateaup (car pos) *x_max*)
               (plateaup (cdr pos) *y_max*))
    (error "Outside of plateau"))
  pos)


(defun update-position (pos orientation cmds)
  (let ((move (car cmds)))
    (if move
        (progn
          (cond
            ((or (eq move #\L) (eq move #\R)) (setq orientation (rotate move orientation)))
            ((eq move #\M) (setq pos (move pos orientation)))
            (t (error "Unknown move")))
          (update-position pos orientation (cdr cmds)))
        (format t "~d ~d ~s~%~%" (car pos) (cdr pos) orientation))))


(defun runme (x y orientation cmds)
  (let ((pos (cons x y)))
    (update-position pos orientation (coerce cmds 'list))))
