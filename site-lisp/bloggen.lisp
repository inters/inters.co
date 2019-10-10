;;;; Simple Blog compiler.

(in-package :inters.co.bloggen)

(let ((days #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
              "Saturday" "Sunday"))
      (months #("January" "February" "March" "April" "May" "June" "July"
                "August" "September" "October" "November" "December")))
  (defun date-string (universal-time)
    (multiple-value-bind (second minute hour date month year day
                          daylight-p zone)
        (decode-universal-time universal-time 0) ; Always GMT
      (declare (ignore second minute hour daylight-p zone))
      (format nil "~a, ~d ~a ~d"
              (aref days day) date (aref months (1- month)) year))))

(defun blog-entries (path)
  (remove-if (lambda (entry)
               (string= "index" (pathname-name entry)))
             (directory
              (make-pathname :defaults path :name :wild :type "meta"))))

(defun read-entries (entries)
  (sort (loop for entry in entries
              for meta = (import-configuration entry)
              for write-date = (getf meta :publication-date
                                     (getf (getf meta :document) :date))
           when write-date collect
             (list (native-namestring
                    (make-pathname :name (pathname-name entry)
                                   :type "html"))
                   (getf (getf meta :document) :title)
                   (parse-date-time write-date)))
        (lambda (x y)
          (> (third x) (third y)))))

(defun index-document (description entries)
  (make-document
   `(,@(read-mk2 description)
     ,(make-listing
       (loop for (href title write-date) in entries collect
            (list (make-url title href) " "
                  (make-italic (date-string write-date))))))))

(defun compile-index (about entries destination)
  (with-open-file (out destination
                       :direction :output
                       :if-exists :supersede)
    (print-mk2 (index-document about entries) out)))

(defun compile-rss (url title entries destination)
  (compile-rss-feed url title entries destination :if-exists :supersede))

(defun bloggen (path)
  (let* ((meta (import-configuration
                (make-pathname :defaults path :type "meta")))
         (blog-meta (getf meta :blog))
         (document-meta (getf meta :document))
         (entries (read-entries (blog-entries path))))
    ;; Compile mk2 index
    (compile-index (getf blog-meta :description)
                   entries
                   (make-pathname :defaults path :type "mk2"))
    ;; Compile RSS feed
    (compile-rss (getf blog-meta :url)
                 (getf document-meta :title)
                 entries
                 (make-pathname :defaults path :type "xml"))))
