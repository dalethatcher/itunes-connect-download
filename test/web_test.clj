(ns web-test
  (:use web :reload)
  (:use clojure.test)
  (:use clojure.contrib.server-socket)
  (:use clojure.java.io)
  (:import [java.io StringWriter InputStream])
)

(defn create-http-response [body]
  (str "HTTP/1.0 200 OK\r\n"
       "Content: text/plain\r\n"
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
  (let [form (get-form "appleConnectForm" (slurp "test-resources/parser-test.html"))]
    (is (= (form :name) "appleConnectForm"))
    (is (= (form :method) "post"))
    (is (= (form :location) "http://localhost:8080/someposturl"))
    (is (= (count (form :arguments)) 6))
    (is (= ((form :arguments) "theAccountName") "a value"))
    (is (= ((form :arguments) "theAccountPW") ""))
    (is (= ((form :arguments) "1.Continue.x") "0"))
    (is (= ((form :arguments) "1.Continue.y") "0"))
    (is (= ((form :arguments) "1.Forgot.x") "0"))
    (is (= ((form :arguments) "1.Forgot.y") "0"))
  )
)
