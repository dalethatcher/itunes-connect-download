(ns itunes-connect-download-test
  (:use [itunes-connect-download] :reload)
  (:use [clojure.test])
  (:use clojure.contrib.trace)
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

(deftest parse-earnings-financial-reports-form-test
  (let [form (parse-earnings-financial-reports-form
               (slurp "test-resources/financial-reports.html"))]
    (is (= (form :name) "mainForm"))
    (is (= (form :location) "https://itunesconnect.apple.com/WebObjects/iTunesConnect.woa/wo/3.0.0.9.7.7.1"))
    (is (= (form :arguments) {"0.0.9.7.7.1.3.1.3.1.1.1.3.1.13" "Earnings"
                              "0.0.9.7.7.1.11.y" "0"
                              "0.0.9.7.7.1.11.x" "0"
                              }))
  )
)

(deftest parse-earnings-forms-test
  (let [forms (parse-earnings-forms
                (slurp "test-resources/financial-reports-earnings.html"))]
    (is (= (forms "Jun-2010-Euro-Zone")
           {:name "mainForm"
            :method "post"
            :location "https://itunesconnect.apple.com/WebObjects/iTunesConnect.woa/wo/4.0.0.9.7.7.1"
            :arguments {"0.0.9.7.7.1.3.1.5.11.1.6.13.1.x" "0"
                        "0.0.9.7.7.1.3.1.5.11.1.6.13.1.y" "0"}
           }))
  )
)
