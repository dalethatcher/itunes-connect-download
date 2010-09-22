(ns itunes-connect-download-test
  (:use [itunes-connect-download] :reload)
  (:use [clojure.test])
)

(def get-url-response (ref "Not set"))
(def get-request-url (ref nil))
(def post-url-response (ref "Not set"))
(def post-request-url (ref nil))
(def post-request-arguments (ref nil))

(defn web-fixture [f]
  (dosync
    (ref-set get-request-url nil)
    (ref-set post-request-url nil)
    (ref-set post-request-arguments nil))
  (binding 
      [web/create-http-client #(eval nil)
       web/get-url (fn [_ url]
                     (dosync (ref-set get-request-url url))
                     @get-url-response)
       web/post-url (fn [_ url arguments]
                      (dosync
                        (ref-set post-request-url url)
                        (ref-set post-request-arguments arguments))
                      @post-url-response)]
    (f)
  )
)

(use-fixtures :each web-fixture)

(deftest get-url-test
)
