
; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node191.html

; these reader macros are here for convenience
; it is safe to remove this file completely from this library.
; neither veq nor weird depend on this file.
; use #+:veq-reader-macros to enable when loading veq/weird

; ignore shebang lines
(set-dispatch-macro-character #\# #\!
  (lambda (stream subchar arg)
    (declare (ignore subchar arg))
    (read-line stream nil (values) t)
    (values)))

; do nothing / ignore this form
(set-dispatch-macro-character #\# #\z
  (lambda (stream subchar arg)
    (declare (ignore subchar arg))
    (read stream t nil t)
    (values)))

; print result
(set-dispatch-macro-character #\# #\q
  (lambda (stream subchar arg)
    (declare (ignorable subchar arg))
    (let ((r (read stream t nil t)))
      (case arg (0 `(veq:vpr ,r))
                (1 `(print ,r))
                (t `(veq:vpr ,r))))))

; (mvc #'values rest)
(set-dispatch-macro-character #\# #\~
  (lambda (stream subchar arg)
    (declare (ignorable subchar arg))
    `(veq:~ ,@(read stream t nil t))))

; expand macro, and execute and or print result
(set-dispatch-macro-character #\# #\m
  (lambda (stream subchar arg)
    (declare (ignorable subchar arg))
    (let ((r (read stream t nil t)))
      (case arg (0 `(veq:mac* ,r))
                (1 `(progn (veq:mac ,r)))
                ; expand, execute and print all values
                (2 `(progn (veq:mac* ,r) (veq:vpr ,r)))
                (3 `(progn (veq:mac ,r) (veq:vpr ,r)))
                ; expand macro
                (t `(veq:mac* ,r))))))


(defun -read-separator (stream char)
  (declare (ignore stream))
  (error "Separator ~S shouldn't be read alone.
next symb: ~a" char (peek-char t stream t nil t)))

(defun -read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone.
next symb: ~a" char (peek-char t stream t nil t)))

(set-macro-character #\} 'read-delimiter)

; adapted from https://gist.github.com/chaitanyagupta/9324402
(defun read-next-object (sep del &optional (stream *standard-input*))
  (flet ((peek- () (peek-char t stream t nil t))
         (discard- () (read-char stream t nil t)))
    (if (and del (char= (peek-) del))
        (progn (discard-) nil)
        (let ((o (read stream t nil t))
              (nxt (peek-)))
          (cond ((char= nxt sep) (discard-))
                ((and del (char= nxt del)) nil)
                (t (error "Unexpected next char: ~S" nxt)))
          o))))

; adapted from https://gist.github.com/chaitanyagupta/9324402
(defun -read-left-brace (stream char)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\Space '-read-separator)
    (loop for object = (read-next-object #\Space  #\} stream)
          while object
          collect object into objects
          finally (return `(values ,@objects)))))

(set-macro-character #\{ '-read-left-brace)

