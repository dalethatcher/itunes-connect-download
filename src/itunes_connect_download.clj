(ns itunes-connect-download
  (:use web)
  (:use [clojure.java.io :only [input-stream]])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:import [java.util Properties])
  (:gen-class))

(def root-url "https://itunesconnect.apple.com")

(defn load-properties [file]
  (println (str "Loading properties from '" file "'."))
  (let [p (Properties.)]
    (do
      (.load p (input-stream file))
      (reduce #(assoc %1 (keyword (.getKey %2)) (.getValue %2)) {} p)
    )
  )
)

(defn login [http-client username password]
  (println (str "Logging in as user '" username "'."))
  (let [login-page (get-url http-client root-url)
        login-form (get-form "appleConnectForm" login-page)
        arg-keys (keys (login-form :arguments))
        required-args (apply dissoc (login-form :arguments)
                             (filter #(.contains % "Forgot") arg-keys))
        args (assoc required-args "theAccountName" username
                                  "theAccountPW" password)]
    (post-url http-client (str root-url (login-form :location)) args)
  )
)

(defn get-sales-and-trends-link [logged-in-page]
  (str root-url
    ((get-links #"Sales and Trends" logged-in-page) "Sales and Trends"))
)

(defn get-financial-reports-link [logged-in-page]
  (let [links (get-links #"Financial Reports" logged-in-page)]
    (str root-url (links (first (keys links))))
  )
)

(defn -main [& args]
  (let [properties (load-properties (str (System/getProperty "user.home")
                                         "/.itunes-download.properties"))
        http-client (create-http-client)
        logged-in-page (login http-client (properties :username)
                              (properties :password))
        financial-reports-link (get-financial-reports-link logged-in-page)
        financial-reports-page (get-url http-client financial-reports-link)]
    (spit (str "financial-reports" (System/currentTimeMillis) ".html")
          financial-reports-page)
  )
)
