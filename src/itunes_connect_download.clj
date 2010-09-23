(ns itunes-connect-download
  (:use web)
  (:gen-class))

(def root-url "https://itunesconnect.apple.com")

(defn login [http-client username password]
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

(defn -main [& args]
  (println "Fetching reports")
)
