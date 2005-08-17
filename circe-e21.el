;;; Emacs 21 backward compatibility defines

(when (not (boundp 'seconds-to-time))
  (defun seconds-to-time (seconds)
    "Convert SECONDS (a floating point number) to a time value."
    (list (floor seconds 65536)
          (floor (mod seconds 65536))
          (floor (* (- seconds (ffloor seconds)) 1000000)))))
