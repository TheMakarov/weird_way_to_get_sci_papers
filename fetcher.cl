(ql:quickload '(:drakma :cl-ppcre))
(ql:quickload :cl-ppcre)

(defun extract-urls (html-string)
  "Extracts all URLs from location.href='...' patterns in HTML content"
  (cl-ppcre:all-matches-as-strings
   "location\\.href\\s*=\\s*['\"]([^'\"]+)['\"]"
   html-string
   :sharedp t))

(defun process-http-addresses (input-file prefix output-file)
  "h7ta tkhdam had lh7abs"
  (with-open-file (in input-file :direction :input)
    (let ((loop_number 1))
      (loop for line = (read-line in nil)
            while line do
              (let* ((full-url (concatenate 'string prefix line))
                     (response (handler-case
                                   (drakma:http-request full-url)
                                 (error (e)
                                   (format t "Error fetching ~A: ~A~%" full-url e)
                                   nil)))
                     )

                (when response
                  (let*
                      (
                       ;; getting the grabbing link
                       (pdf_link_ (car (extract-urls response)))

                       ;; tempo
                       (pdf_link_
                         (subseq
                          (car
                           (extract-urls response)) 17
                          (- (length pdf_link_) 1)
                          ))

                       (pdf_link
                         (concatenate 'string "http://" pdf_link_))

                       (pdf_file (handler-case
                                     (drakma:http-request pdf_link)
                                   (error (e)
                                     (format t "Error fetching ~A: ~A~%" pdf_link e)
                                     nil))))

                    ;; why this worked and the format didn't ?
                    (when pdf_file
                      (with-open-file (out2 (format nil "~A_~D.pdf" output-file loop_number)
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :element-type '(unsigned-byte 8))
                      (write-sequence pdf_file out2)))

                    (setq loop_number (1+ loop_number))
                    )))))))
