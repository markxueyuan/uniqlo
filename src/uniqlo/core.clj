(ns uniqlo.core
  (:import (java.io ByteArrayOutputStream File InputStreamReader StringReader)
           (javax.imageio ImageIO)
           (java.net URL)
           [org.openqa.selenium WebDriver Cookie OutputType TakesScreenshot]
           [org.openqa.selenium WebElement]
           [org.openqa.selenium.support.ui ExpectedCondition ExpectedConditions]
           [org.openqa.selenium.support.ui WebDriverWait]
           (org.openqa.selenium.firefox FirefoxDriver FirefoxProfile)
           (org.openqa.selenium By)
           (org.openqa.selenium Cookie)
           (org.openqa.selenium JavascriptExecutor)
           (org.openqa.selenium.htmlunit HtmlUnitDriver)
           (org.openqa.selenium.firefox.internal ProfilesIni)
           (java.io File)
           (org.openqa.selenium.ie InternetExplorerDriver)
           (org.openqa.selenium.chrome ChromeDriver)
           (org.openqa.selenium.remote Augmenter)
           (java.util UUID))
  (:require [mikera.image.core :as imagez]
            [clojure.java.io :as io]
            [net.cgrand.enlive-html :as html]
            [clojure.string :as string]
            [clojure-csv.core :as clj-csv]
            [clojure.data.csv :as csv]))


(def ^:dynamic my-driver nil)

(defmacro use-firefox
  []
  `(def my-driver (FirefoxDriver.)))

(defmacro use-htmlunit
  []
  `(def my-driver (HtmlUnitDriver.)))

(defmacro use-ie
  []
  (let [_ (System/setProperty
            "webdriver.ie.driver"
            "resources/driver/IEDriverServer.exe")]
    `(def my-driver (InternetExplorerDriver.))))

(defmacro use-chrome
  []
  (let [_ (System/setProperty
            "webdriver.chrome.driver"
            "resources/driver/chromedriver.exe")]
    `(def my-driver (ChromeDriver.))))

(defn firefox
  []
  (FirefoxDriver.))

(defn htmlunit
  []
  (HtmlUnitDriver.))

(defn ie
  []
  (let [_ (System/setProperty
            "webdriver.ie.driver"
            "C:/iedriver/IEDriverServer.exe")]
    (InternetExplorerDriver.)))

(defn chrome
  []
  (let [_ (System/setProperty
            "webdriver.chrome.driver"
            "C:/chromedriver/chromedriver.exe")]
    (ChromeDriver.)))

(defn to
  [url]
  (.get my-driver url))

(defn execute-script
  [^String script]
  (.executeScript ^JavascriptExecutor my-driver script (make-array Object 0)))

(defn freeze
  []
  (-> (execute-script "return document.documentElement.outerHTML;")
      StringReader.
      html/html-resource))

(defn write-csv-quoted
  [coll file & {:keys [append encoding]}]
  (let [keys-vec (keys (first coll))
        vals-vecs (map (apply juxt keys-vec) coll)]
    (with-open [out (io/writer file :append append :encoding encoding)]
      (binding [*out* out]
        (when-not append
          (print (clj-csv/write-csv (vector (map name keys-vec)) :force-quote true)))
        (doseq [v vals-vecs]
          (let [v (map str v)]
            (print (clj-csv/write-csv (vector v) :force-quote true))))))))

(defn lazy-read-csv
  [csv-file]
  (let [in-file (io/reader csv-file)
        csv-seq (csv/read-csv in-file)
        lazy (fn lazy [wrapped]
               (lazy-seq
                 (if-let [s (seq wrapped)]
                   (cons (first s) (lazy (rest s)))
                   (.close in-file))))]
    (lazy csv-seq)))

(defn lazy-read-csv-head-on
  [file]
  (let [coll (lazy-read-csv file)
        head (map keyword (first coll))
        rows (rest coll)]
    (map #(zipmap head %) rows)))


(defn get-url
  [url]
  (let [html (html/html-resource (URL. url))
        items (html/select html [:.permalink])]
    (-> (map #(get-in % [:attrs :href]) items)
        distinct)))

(defn get-url-new
  [html]
  (let [items (html/select html [:.permalink])]
    (-> (map #(get-in % [:attrs :href]) items)
        distinct)))

(defn get-url2
  [html]
  (let [items (html/select html [:.main-wrap :a])]
    (-> (map #(get-in % [:attrs :href]) items)
        distinct)))


(defn get-html-old
  [url & {:keys [encoding] :or {encoding "gbk"}}]
  (-> (URL. url)
      .getContent
      (InputStreamReader. encoding)
      html/html-resource))

(defn get-html-old
  [url & {:keys [encoding] :or {encoding "gbk"}}]
  (-> (URL. url)
      .getContent
      (InputStreamReader. encoding)
      html/html-resource))


(defn get-product-src
  [html]
  (->> (html/select html [:.zoom-cache :img])
       (map #(get-in % [:attrs :src]))))

(defn get-model-src
  [html]
  (let [src (-> (html/select html [:#J_DivItemDesc :table])
                second
                (html/select [:img]))]
    (map #(get-in % [:attrs :src]) src)))

(defn get-title
  [html]
  (-> (html/select html [:.detail-hd])
      first
      html/text
      string/trim))

(defn get-description
  [html]
  (let [content (-> (html/select html [:#J_DivItemDesc :table])
                    first
                    (html/select [:p]))]
    (->> (html/texts content)
         (remove #(= "" %))
         (map string/trim)
         (string/join "////"))))

(defn get-attribute
  [html]
  (->> (html/select html [:#attributes :ul :li])
       (map html/text)
       (map string/trim)
       (string/join "////")
       ))

(defn get-price
  [html]
  (-> (html/select html [:#J_StrPrice])
      first
      (html/text)))

(defn save-picture
  [url file]
  (with-open [in (io/input-stream url)
              out (io/output-stream file)]
    (io/copy in out)))

(defn get-meta
  [html]
  {:uuid        (UUID/randomUUID)
   :title       (get-title html)
   :description (get-description html)
   :attribute   (get-attribute html)
   :price       (get-price html)
   })

(defn get-picture
  [html fold meta]
  (let [products (get-product-src html)
        models (get-model-src html)
        uuid (:uuid meta)
        c1 (atom 1)
        c2 (atom 1)]
    (doseq [p products]
      (save-picture p (str fold uuid "_product_" (format "%03d" @c1) ".jpg"))
      (swap! c1 inc)
      (Thread/sleep (+ 5 (rand-int 5))))
    (doseq [m models]
      (save-picture m (str fold uuid "_model_" (format "%03d" @c2) ".jpg"))
      (swap! c2 inc)
      (Thread/sleep (+ 5 (rand-int 5))))))

(defn run
  [url fold]
  (to url)
  (Thread/sleep 3000)
  (let [urls (get-url-new (freeze))
        u1 (first urls)
        urls (rest urls)
        file (str fold "meta.csv")]
    (to u1)
    (Thread/sleep 5000)
    (let [html (freeze)
          meta (get-meta html)]
      (write-csv-quoted [meta] file)
      (get-picture html fold meta)
      (println "Initiated!"))
    (doseq [u urls]
      (try
        (to u)
        (Thread/sleep 5000)
        (let [html (freeze)
              meta (get-meta html)]
          (write-csv-quoted [meta] file :append true)
          (get-picture html fold meta)
          (println "Succeeded!"))
        (catch Throwable e (println "OOPS!"))))))

(defn smart-run
  [url fold]
  (binding [my-driver (firefox)]
    (run url fold)))


(defn start
  [job fold]
  (doseq [j (lazy-read-csv job)]
    (.mkdir (io/as-file (str fold (first j) "/")))
    (run (second j) (str fold (first j) "/"))
    ))

(defn smart-start
  [job fold]
  (binding [my-driver (firefox)]
    (start job fold)))
