(load "mars-rover.lisp")

(defun set-plateau-size (plateau-shape)
  (setf *x_max* (car plateau-shape))
  (setf *y_max* (cadr plateau-shape)))


(defun loop-over-starting-positions (input)
  "Loop over the starting poition/orientation and commands

  INPUT is a list of pairs of lists (x y orientation) (commands)"
  (when input
    (let* ((start (car input))
           (commands (coerce (string (car (cadr input))) 'list))
           (pos (cons (car start) (cadr start)))
           (orientation (caddr start)))
      (update-position pos orientation commands))
    (loop-over-starting-positions (cddr input))))


(defun read-input-file (filename)
  ;; Throw away blank lines
  (let ((setup (loop for line in (uiop:read-file-lines filename)
                  if (not (equal line ""))
                  collect (read-from-string (concatenate 'string "(" line ")")))))
    (set-plateau-size (car setup))
    (loop-over-starting-positions (cdr setup))))
