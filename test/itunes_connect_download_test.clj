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

(deftest load-properties-test 
  (let [properties (load-properties "test-resources/sample.properties")]
    (is (= "somevalue" (properties :somekey)))
  )
)

(deftest login-test
  (dosync
    (ref-set get-url-response (slurp "test-resources/login-form.html"))
    (ref-set post-url-response (slurp "test-resources/logged-in.html")))
  (is (= @post-url-response (login nil "user" "pass")))
  (is (= @get-request-url "https://itunesconnect.apple.com"))
  (is (= @post-request-url "https://itunesconnect.apple.com/WebObjects/iTunesConnect.woa/wo/0.0.9.3.3.2.1.1.3.1.1"))
  (is (= (count @post-request-arguments) 4))
  (is (= (@post-request-arguments "theAccountName") "user"))
  (is (= (@post-request-arguments "theAccountPW") "pass"))
  (is (= (@post-request-arguments "1.Continue.x") "0"))
  (is (= (@post-request-arguments "1.Continue.y") "0"))
)
