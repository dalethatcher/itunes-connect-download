(ns itunes-connect-download
  (:use web)
  (:use [clojure.java.io :only [input-stream]])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use clojure.contrib.trace)
  (:import [java.util Properties]
           [java.io File])
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

(defn parse-earnings-financial-reports-form [financial-reports-page]
  (let [form (get-form "mainForm" financial-reports-page)
        new-args (into {} (filter #(some (partial = (second %)) ["Earnings" "0"])
                         (form :arguments)))
        location (str root-url (form :location))]
    (assoc form :arguments new-args :location location)
  )
)

(def months #{"Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"})

(defn- key-from-earnings-row [row]
  (str (.replace (first row) " " "-") "-" (second row))
)

(defn parse-earnings-forms [earnings-page]
  (let [table-form (get-table-form "mainForm" earnings-page)
        date-rows (filter #(months (first (.split (str (first %)) " ")))
                          (table-form :arguments))]
    (apply hash-map
      (mapcat #(vec [(key-from-earnings-row %)
                     (assoc table-form :arguments (nth % 5)
                            :location (str root-url (table-form :location)))])
              date-rows))
  )
)

(defn- directory-if-exists [path]
  (let [f (File. path)]
    (if (.isDirectory f)
      f
      (do
        (println "Directory" path "doesn't exist, aborting!")
        (System/exit -1)
      )
    )
  )
)

(defn- fetch-report-if-new [http-client reports-directory [report-name report-form]]
  (let [file-name (str report-name ".txt.gz")
        file (File. reports-directory file-name)]
    (if (.exists file)
      (println (str "Skipping " file-name "."))
      (do
        (println (str "Fetching " file-name "."))
        (post-url-with-download http-client
                                (report-form :location)
                                (report-form :arguments)
                                (.getAbsolutePath file))
      )
    )
  )
)

(defn -main [& args]
  (let [properties (load-properties (str (System/getProperty "user.home")
                                         "/.itunes-download.properties"))
        reports-directory (directory-if-exists (properties :reports))
        http-client (create-http-client)
        logged-in-page (login http-client (properties :username)
                              (properties :password))
        financial-reports-link (get-financial-reports-link logged-in-page)
        financial-reports-page (get-url http-client financial-reports-link)
        earnings-list-form (parse-earnings-financial-reports-form financial-reports-page)
        earnings-page (post-url http-client (earnings-list-form :location)
                                (earnings-list-form :arguments))
        earnings-forms (parse-earnings-forms earnings-page)
        [report-name report-form] (first earnings-forms)]
    (doall
      (map #(fetch-report-if-new http-client reports-directory %) earnings-forms))
  )
)
