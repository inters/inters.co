;; Compile documents from META hints.

(in-package :inters.co.metagen)

(defun compile-api-document (packages path)
  (with-open-file (out (make-pathname :defaults path :type "mk2")
                       :direction :output
                       :if-exists :supersede)
    (print-mk2 (apply #'api-document packages) out)))

(defun metagen (path)
  (destructuring-bind (&key system api-document)
      (getf (import-configuration
             (make-pathname :defaults path :type "meta"))
            :source)
    (when system (quickload system))
    (when api-document (compile-api-document api-document path))))
