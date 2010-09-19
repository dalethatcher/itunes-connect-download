(ns web
  (:import (org.apache.http.client ResponseHandler)
           (org.apache.http.impl.client DefaultHttpClient BasicResponseHandler)
           (org.apache.http.client.params ClientPNames CookiePolicy)
           (org.apache.http.client.methods HttpGet HttpPost)
           (org.apache.http.client.entity UrlEncodedFormEntity)
           (org.apache.http.protocol HTTP)
           (org.apache.http.message BasicNameValuePair)
           (java.io File FileOutputStream)
           (java.util.regex Pattern)
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
