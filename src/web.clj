(ns web
  (:import (org.apache.http.impl.client DefaultHttpClient BasicResponseHandler)
           (org.apache.http.client.params ClientPNames CookiePolicy)
           (org.apache.http.client.methods HttpGet HttpPost)
           (org.apache.http.client.entity UrlEncodedFormEntity)
           (org.apache.http.protocol HTTP)
           (org.apache.http.message BasicNameValuePair)
           (org.htmlparser Parser NodeFilter)
           (org.htmlparser.lexer Lexer)
           (org.htmlparser.nodes TextNode)
           (org.htmlparser.tags FormTag LinkTag TableRow InputTag TableColumn)
           )
  (:use clojure.contrib.trace)
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
    (if (or (nil? node-list) (zero? (.size node-list)))
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

(defn- input-to-argument [input-node]
  (let [name (.getAttribute input-node "name")
        value (.getAttribute input-node "value")
        arg-value (if (nil? value) "" value)
        type (.getAttribute input-node "type")]
    (if (= type "image")
      {(str name ".x") "0" (str name ".y") "0"}
      {name arg-value})
  )
)

(defn- form-inputs-to-arguments [node-list-seq]
  (reduce (fn [m node]
            (into m (input-to-argument node)))
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

(defn- node-filter [f]
  (proxy [NodeFilter] []
    (accept [node]
            (f node)
    )
  )
)

(defn- get-child-input-node [node]
  (let [f (node-filter #(instance? InputTag %))
        children (.getChildren node)
        found-nodes (if children (.extractAllNodesThatMatch children f true))]
    (first (node-list-to-seq found-nodes))
  )
)

(defn- child-text [node]
  (if-let [children (node-list-to-seq (.getChildren node))]
    (.trim
      (reduce str ""
              (map #(.getText %)
                   (filter #(instance? TextNode %) children))))
    "")
)

(defn- table-row-to-arguments [table-row]
  (let [children (node-list-to-seq (.getChildren table-row))
        td-children (filter #(instance? TableColumn %) children)]
    (map (fn [td]
           (if-let [input-node (get-child-input-node td)]
             (input-to-argument input-node)
             (child-text td)
           )
         )
         td-children)
  )
)

(defn- table-rows-to-arguments [table-rows]
  (reduce (fn [a r]
            (conj a (table-row-to-arguments r)))
          #{}
          (node-list-to-seq table-rows))
)

(defn- form-table-node-to-map [#^FormTag form-node]
  (let [f (node-filter #(instance? TableRow %))
        table-rows (.extractAllNodesThatMatch (.getChildren form-node) f true)]
    {:name (.getFormName form-node)
     :method (.getFormMethod form-node)
     :location (.getFormLocation form-node)
     :arguments (table-rows-to-arguments table-rows)
     }
  )
)

(defn- matching-nodes [f page]
  (let [parser (Parser. (Lexer. page))
        found-nodes (.extractAllNodesThatMatch parser (node-filter f))]
    (node-list-to-seq found-nodes)
  )
)

(defn- get-form-nodes-with-name [form-name page]
  (matching-nodes (fn [node]
                    (and (instance? FormTag node)
                         (= form-name (.getFormName node)))) page)
)

(defn  get-form [form-name page]
  (let [found-nodes (get-form-nodes-with-name form-name page)]
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

(defn get-table-form [form-name page]
  (let [found-nodes (get-form-nodes-with-name form-name page)]
    (if (= (count found-nodes) 0)
      nil
      (form-table-node-to-map (first found-nodes))
    )
  )
)

