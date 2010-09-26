(ns web
  (:import (org.apache.http.impl.client DefaultHttpClient BasicResponseHandler)
           (org.apache.http.client.params ClientPNames CookiePolicy)
           (org.apache.http.client.methods HttpGet HttpPost)
           (org.apache.http.client.entity UrlEncodedFormEntity)
           (org.apache.http.protocol HTTP)
           (org.apache.http.message BasicNameValuePair)
           (org.htmlparser Parser NodeFilter)
           (org.htmlparser.lexer Lexer)
           (org.htmlparser.tags FormTag LinkTag)
           )
)

(defn- fetch-result-of-method
  "Fetches the result of the given method."
  [http-client method]
  (let [response-handler (BasicResponseHandler.)]
    (.execute http-client method response-handler)
  )
)

(defn- create-http-form-entity
  "Create a http form entity object with the given arguments"
  [arguments]
  (let [value-pairs (map (fn [[key value]] (BasicNameValuePair. key value)) arguments)]
    (UrlEncodedFormEntity. value-pairs HTTP/UTF_8)
  )
)

(defn- create-post-method
  "Creates a post method with the given arguments."
  [url-string arguments]
  (doto (HttpPost. url-string)
      (.setEntity (create-http-form-entity arguments))
  )
)

(defn- set-cookie-browser-compatibility
  "Sets a cookie handling parameter to match Apple's dodgy use of them."
  [http-client]
  (let [parameters (.getParams http-client)]
      (.setParameter parameters ClientPNames/COOKIE_POLICY CookiePolicy/BROWSER_COMPATIBILITY)
      http-client
  )
)

(defn create-http-client
  "Creates a HTTP client that must be passed into all other methods."
  []
  (set-cookie-browser-compatibility (DefaultHttpClient.))
)

(defn post-url
  "Posts the arguments to the given URL and returns the result."
  [http-client url-string arguments]
  (do
    (fetch-result-of-method http-client (create-post-method url-string arguments))
  )
)

(defn get-url
  "Gets the body of the given URL"
  [http-client url-string]
  (do
    (fetch-result-of-method http-client (HttpGet. url-string))
  )
)

(defn- node-list-to-seq
  ([node-list]
    (if (zero? (.size node-list))
      []
     (node-list-to-seq node-list 0 (.size node-list)))
  )
  ([node-list n max]
   (if (>= n max)
     []
     (lazy-seq
       (cons (.elementAt node-list n) (node-list-to-seq node-list (inc n) max))))
  )
)

(defn- form-inputs-to-arguments [node-list-seq]
  (reduce (fn [m node]
      (let [name (.getAttribute node "name")
            value (.getAttribute node "value")
            arg-value (if (nil? value) "" value)
            type (.getAttribute node "type")]
        (apply assoc m
          (if (= type "image")
            [(str name ".x") "0" (str name ".y") "0"]
            [name arg-value]))
      ))
    {}
    node-list-seq
  )
)


(defn- form-node-to-map [#^FormTag form-node]
  (let [input-nodes (node-list-to-seq (.getFormInputs form-node))]
    {:name (.getFormName form-node)
     :method (.getFormMethod form-node)
     :location (.getFormLocation form-node)
     :arguments (form-inputs-to-arguments input-nodes)
     }
  )
)

(defn- matching-nodes [f page]
  (let [parser (Parser. (Lexer. page))
        node-filter (proxy [NodeFilter] []
                      (accept [node]
                        (f node)
                      )
                    )
        found-nodes (.extractAllNodesThatMatch parser node-filter)]
    (node-list-to-seq found-nodes)
  )
)

(defn get-form [form-name page]
  (let [found-nodes (matching-nodes (fn [node]
                        (and (instance? FormTag node)
                            (= form-name (.getFormName node)))) page)]
    (if (> (count found-nodes) 0)
      (form-node-to-map (first found-nodes))
      nil)
  )
)

(defn get-links [re page]
  (let [links (matching-nodes
                  (fn [node]
                    (and (instance? LinkTag node)
                         (not (nil? (.getLinkText node)))
                         (not (nil? (re-find re (.getLinkText node))))
                    )
                  )
                  page)]
    (reduce #(assoc %1 (.trim (.getLinkText %2)) (.getLink %2)) {} links)
  )
)
(get-links #"Sales and Trends" (slurp "test-resources/logged-in.html"))
