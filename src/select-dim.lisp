
(in-package :veq)


; vdef* renames all symbols inside the function that are the same as the
; function name. so we use x* instead of x etc.
(vdef* x (x* &rest rest) (declare #.*opt* (ignorable rest)) x*)
(vdef* y (x y* &rest rest) (declare #.*opt* (ignorable x rest)) y*)
(vdef* z (x y z* &rest rest) (declare #.*opt* (ignorable x y rest)) z*)

(vdef* xy (x y &rest rest) (declare #.*opt* (ignorable rest)) (values x y))
(vdef* xz (x y z &rest rest) (declare #.*opt* (ignorable y rest)) (values x z))
(vdef* yz (x y z &rest rest) (declare #.*opt* (ignorable x rest)) (values y z))
(vdef* yx (x y &rest rest) (declare #.*opt* (ignorable rest)) (values y x))
(vdef* zx (x y z &rest rest) (declare #.*opt* (ignorable y rest)) (values z x))
(vdef* zy (x y z &rest rest) (declare #.*opt* (ignorable x rest)) (values z y))

(vdef* xzy (x y z &rest rest) (declare #.*opt* (ignorable rest)) (values x z y))
(vdef* xyz (x y z &rest rest) (declare #.*opt* (ignorable rest)) (values x y z))
(vdef* zxy (x y z &rest rest) (declare #.*opt* (ignorable rest)) (values z x y))
(vdef* zyx (x y z &rest rest) (declare #.*opt* (ignorable rest)) (values z y x))
(vdef* yxz (x y z &rest rest) (declare #.*opt* (ignorable rest)) (values y x z))
(vdef* yzx (x y z &rest rest) (declare #.*opt* (ignorable rest)) (values y z x))

