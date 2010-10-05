;; Copyright 2010 Dale Thatcher. All rights reserved.
;;
;; iTunes Connect Download is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; iTunes Connect Download is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;
;; See the GNU Affero General Public License for more details.
;;
;; Please see the LICENSE file for a copy of the GNU Affero General Public License.

(ns web-test
  (:use web :reload)
  (:use clojure.test)
  (:use clojure.contrib.server-socket)
  (:use clojure.java.io)
  (:import [java.io StringWriter InputStream File])
)

(defn create-http-response [body & headers]
  (str "HTTP/1.0 200 OK\r\n"
       "Content: text/plain\r\n"
       (apply str headers)
       "\r\n"
       body)
)

(def response-data (ref (create-http-response "Not set")))
(def request-data (ref nil))

(defn read-header-line [request-chars]
  (loop [s (new StringBuilder)
         chars request-chars]
    (let [c (first chars)]
      (if (= '\newline c)
        [(rest chars) (-> s (.append "\n") .toString)]
        (recur (.append s c) (rest chars))
      )
    )
  )
)

(defn get-length [h]
  (let [length-string (second (.split h ":"))]
    (Integer. (.trim length-string))
  )
)

(defn read-header [request-chars]
  (loop [length 0
         chars request-chars]
    (let [[remaining-chars line] (read-header-line chars)]
      (cond
        (.startsWith line "Content-Length: ")
          (recur (get-length line) remaining-chars)
        (<= (.length line) 2)
          [remaining-chars length]
        :else
          (recur length remaining-chars)
      )
    )
  )
)

(defn to-char-seq [#^InputStream in]
  (take-while #(not (nil? %))
      (repeatedly #(try (char (.read in)) (catch Exception _ nil)))
  )
)

(defn read-input [in]
  (let [request-chars (to-char-seq in)
        [remaining-chars content-length] (read-header request-chars)]
    (apply str (take content-length remaining-chars))
  )
)

(defn answer-request [in out]
  (dosync
    (ref-set request-data (read-input in)))
  (doto out
    (.write (.getBytes @response-data))
    (.close)
  )
)

(defn start-socket-listener [f]
  (let [server (create-server 9090 answer-request)]
    (try
      (f)
      (finally
        (close-server server))
    )
  )
)

(use-fixtures :once start-socket-listener)

(deftest can-get-url
  (dosync (ref-set response-data (create-http-response "Get response")))
  (is (= (get-url (create-http-client) "http://localhost:9090")
         "Get response"))
)

(deftest can-post-url
  (dosync (ref-set response-data (create-http-response "Post response")))
  (is (= (post-url (create-http-client) "http://localhost:9090" {"a" "1"})
         "Post response"))
  (is (= @request-data "a=1"))
)

(deftest can-extract-html-forms
  (let [form (get-form "appleConnectForm" (slurp "test-resources/login-form.html"))]
    (is (= (form :name) "appleConnectForm"))
    (is (= (form :method) "post"))
    (is (= (form :location) "/WebObjects/iTunesConnect.woa/wo/0.0.9.3.3.2.1.1.3.1.1"))
    (is (= (count (form :arguments)) 6))
    (is (= ((form :arguments) "theAccountName") ""))
    (is (= ((form :arguments) "theAccountPW") ""))
    (is (= ((form :arguments) "1.Continue.x") "0"))
    (is (= ((form :arguments) "1.Continue.y") "0"))
    (is (= ((form :arguments) "1.Forgot.x") "0"))
    (is (= ((form :arguments) "1.Forgot.y") "0"))
  )
)

(deftest can-extract-html-links
  (let [links (get-links #"Sales and Trends" (slurp "test-resources/logged-in.html"))]
    (is (= (count links) 1))
    (is (= "/WebObjects/iTunesConnect.woa/wo/0.0.9.7.2.9.1.0.0.3"
           (links "Sales and Trends")))
  )
)

(deftest can-extract-table-forms
  (let [form (get-table-form "mainForm"
                             (slurp "test-resources/financial-reports-earnings.html"))]
    (is (= (form :name) "mainForm"))
    (is (= (form :method) "post"))
    (is (= (count (form :arguments)) 16))
    (is (not (nil? (some #{["Aug 2010" "Euro-Zone" "0" "0.00" "EUR"
                            {"0.0.9.7.7.1.3.1.5.11.1.0.13.1.x" "0"
                             "0.0.9.7.7.1.3.1.5.11.1.0.13.1.y" "0"}]}
                     (form :arguments)))))
  )
)

(deftest can-save-post-result-to-file
  (let [output-file (File. "test-output")]
    (try
      (.delete output-file)
      (dosync
        (ref-set response-data
                 (create-http-response "ABC" "filename: A_FILE\r\n")))
      (post-url-with-download (create-http-client)
                              "http://localhost:9090"
                              {"a" "1"}
                              (.getPath output-file))
      (is (= (slurp output-file) "ABC"))
    (finally
      (.delete output-file)))
  )
)
