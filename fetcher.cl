(ql:quickload '(:drakma :cl-ppcre))
(ql:quickload :cl-ppcre)


(defun find-title (html-string)
  (let*
      ((index_first_title (search "i>" html-string )) (index_last_title (search "</i" html-string )))

    (if (equal index_first_title nil ) "not found" (subseq html-string (+ index_first_title 2) index_last_title) )
    ))

(defun sanitize-string (title)
  "this function is made to sanitize title for path"
  (substitute  #\  #\/ title )
  )

(defun construct-title (title)

  (let  (
         (sanitized-title (sanitize-string title)))
    (if (> (length sanitized-title) 255)
        (subseq sanitized-title 0 250)
        sanitized-title
        ))
  )



(defun extract-urls (html-string)
  "Extracts all URLs from location.href='...' patterns in HTML content somehow "
  (let
      ((matched-url (car (cl-ppcre:all-matches-as-strings
                          "location\\.href\\s*=\\s*['\"]([^'\"]+)['\"]"
                          html-string
                          :sharedp t))))

    (if (equal matched-url  nil)
        :non_found
        matched-url
        )
    ))


(defun process-http-addresses (&key input-file (prefix "https://sci-hub.se/")  (output-path "./") (log-file "./err.log") )
  "main"
  ;; for some reason it's doesn't output it ? yes yes yes it does !
  (with-open-file (out log-file
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create
                       )
    (with-open-file (in input-file :direction :input)
      (let ((loop_number 1))
        (loop for line = (read-line in nil)
              while line do
                (let* ((full-url (concatenate 'string prefix line))
                       (response (handler-case
                                     (drakma:http-request full-url)
                                   (error (e)
                                     (format out "Error fetching >~A< : ~A~%" full-url e)
                                     (format t "Error fetching> ~A < : ~A~%" full-url e)
                                     nil)))
                       )

                  (when response
                    (let*
                        (
                         ;; getting the grabbing link
                         (pdf_link_ (extract-urls response))
                         )

                      (case pdf_link_
                        (:non_found
                         (format out "Error fetching webLink  >~D< : ~A , link => \"~A\"~%" 404 "not found" full-url)
                         )
                        (t
                         (let*
                             (
                              (pdf_title
                                (construct-title (find-title response)))
                              (pdf_link_ (subseq
                                          pdf_link_
                                          17
                                          (- (length pdf_link_) 1
                                             )))

                              (pdf_link
                                (concatenate 'string "http://" pdf_link_)
                                )
                              (pdf_file (handler-case
                                            (drakma:http-request pdf_link)
                                          (error (e)
                                            (format out "Error fetching pdf  >~A< : ~A~%" pdf_link e)
                                            nil)))
                              )

                           ;; why this worked and the format didn't ?
                           (when pdf_file
                             (with-open-file (out2 (format nil "~A~Apdf" output-path pdf_title)
                                                   :direction :output
                                                   :if-exists :supersede
                                                   :if-does-not-exist :create
                                                   :element-type '(unsigned-byte 8))
                               (write-sequence pdf_file out2)))


                           (setq loop_number (1+ loop_number)))))))))))))
