(ns sdfx.main
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [hiccup2.core :as h]
            [org.httpkit.server :as srv]
            [ruuter.core :as ruuter]
            [sdfx.geom :as g]
            [sdfx.util :as u]
            [svg-clj.composites :refer [svg]]
            [svg-clj.path :as path]
            [svg-clj.tools :as tools]
            [svg-clj.transforms :as tf]
            [svg-clj.utils :as svg.u])
  (:import [java.net ServerSocket BindException])
  (:gen-class))

(def starting-sdf-ns
  '(ns sdfx.scratch
     (:require [sdfx.geom :as g]
               [svg-clj.parametric :as p])))

(def starting-sdf-src
  '(-> (reduce
        g/union
        [(-> (g/sphere 50) (g/translate [-150 0 0]))
         (-> (g/box 50 50 50) (g/translate [0 0 0]))
         (-> (g/circle 50) (g/extrude 50) (g/translate [150 0 0]))])
       (g/translate [0 0 0])
       (g/rotate [45 35 0])
       (g/slices 5 0.1)))

(def starting-sdf-fn (eval starting-sdf-src))

(defonce ^:private last-src-str (atom nil))
(defonce ^:private contour-history (atom []))
(defonce ^:private show-docs (atom false))
(defonce ^:private show-gallery (atom false))
(defonce ^:private render-settings
  (atom {:colors "contours"
         :outlines false}))

(defn- compile-sdf
  [shapefn render-settings]
  (let [{:keys [colors outlines]} render-settings
        contour-cols              (= "contours" colors)
        normal-cols               (not contour-cols)
        outlines                  (= "on" outlines)]
    (format "bool normalCols  = %s;
bool contourCols = %s;
bool outlines = %s;
float mySdf (in vec3 p) {
  return %s;
}"
            normal-cols
            contour-cols
            outlines
            (shapefn "p"))))

(defn- update-contour-history!
  [entry]
  (let [history @contour-history
        f (if (> (count history) 99)
            (comp #(vec (rest %1)) #(conj %1 %2))
            #(conj %1 %2))]
    (swap! contour-history f entry)))

(defn- strip-do
  [s]
  (if-let [[_ inner-content] (re-matches #"\(do\s+([\s\S]+)\)\s*$" s)]
    inner-content
    s))

(defn- simple-code-printer
  [quoted-expr-or-string]
  (with-out-str
    (pprint/with-pprint-dispatch
      pprint/code-dispatch
      (pprint/pprint
       (edn/read-string (str quoted-expr-or-string))))))

(defn- template
  []
  (list
   "<!DOCTYPE html>"
   (str
    (h/html
        [:head
         [:meta {:charset "UTF-8"}]
         [:meta {:name    "viewport"
                 :content "width=device-width, initial-scale=1"}]
         ;; HTMX stylesheet
         [:link {:rel "stylesheet" :href "resources/missing.min.css"}]
         ;; Codemirror stylesheets
         [:link {:rel "stylesheet" :href "resources/codemirror.min.css"}]
         [:link {:rel "stylesheet" :href "resources/nord.min.css"}]
         ;; SDFx stylesheet
         [:style "#code-container { height: 1100px }
                  @media (max-width: 1200px) {
                    #editor-container { flex-direction: column; } #code-container { height: 300px; }
                  }"]
         [:title "SDFx"]]
        [:body
         [:main
          {:style {:padding 0}}
          ;; sdf src that gets compiled into the fragment Shader
          ;; Needs the container div because the mutation observer looks at the container
          ;; when the #sdf-src div is swapped, then the mutation is observed. If you observe
          ;; the #sdf-src directly, the mutation is not observed because HTMX is swapping a new node in,
          ;; and the mutation observer is attached to the old one, so nothing ever happens
          [:div#sdf-src-container
           [:div#sdf-src {:hx-get     "/src"
                          :hx-trigger "contentChanged"
                          :hx-vals    "js:{ 'sdf-src': editor.getValue() }"
                          :style      {:display "none"}}
            (if-let [src-str nil #_@last-src-str]
              (compile-sdf (eval (read-string src-str)) @render-settings)
              (compile-sdf starting-sdf-fn @render-settings))]]
          [:h2.center {:style {:margin "0.5rem"}} "SDFx"]
          [:div#editor-container.center.f-row.justify-content:center
           {:style {:gap "0px"}}
           ;; editor
           [:div#code-container.center
            {:style {:border "1px solid white"}}
            [:textarea#editor
             {:style {:width  "700px" :height "100%"
                      :border "1px solid white"}
              :type  "text"
              :name  "sdf-src"}
             (if-let [src-str @last-src-str]
               (strip-do src-str)
               (str
                (simple-code-printer starting-sdf-ns) "\n"
                (simple-code-printer starting-sdf-src)))]]
           ;; canvas
           [:div.center.f-col.align-items:start
            {:style {:gap "0px"}}
            [:form#render-settings.margin.tool-bar {:style {:height 0 :z-index 1000}}
             [:input#contour-cols {:hx-post "/settings"
                                   :hx-swap "none"
                                   :type    "radio"
                                   :name    "colors"
                                   :value   "contours"
                                   :checked (= "contours" (:colors @render-settings))}]
             [:label {:for "contour-cols"} "Z-Contour Color"]
             [:input#normal-cols {:hx-post "/settings"
                                  :hx-swap "none"
                                  :type    "radio"
                                  :name    "colors"
                                  :value   "normals"
                                  :checked (= "normals" (:colors @render-settings))}]
             [:label {:for "normal-cols"} "Normal Color"]
             [:input#outlines {:hx-post "/settings"
                               :hx-swap "none"
                               :type    "checkbox"
                               :name    "outlines"
                               :checked (:outlines @render-settings)}]
             [:label {:for "outlines"} "Outlines"]]
            [:canvas#canvas
             {:style {:width  "700px" :height "700px"
                      :border "1px solid white"}
              :width 1400 :height 1400}]
            [:div#contour.center
             {:style {:width  "700px" :height "400px"
                      :border "1px solid white"}}]]]
          [:div.center
           [:section.tool-bar.margin
            [:button {:hx-get    "/docs"
                      :hx-target "#docs"} "Docs"]
            [:hr]
            [:button {:type "button"
                      :onclick "grabPixels()"} "Get Contours"]
            [:hr]
            [:button {:hx-get    "/gallery"
                      :hx-target "#gallery"} "Gallery"]
            [:hr]
            [:form
             [:label "filename: " [:input {:type "text" :name "filename" :style {:width "150px" :margin-right "16px"}}]]
             [:button {:hx-post   "/save"
                       :hx-target "#save-status"} "Save"]]
            [:div#save-status]]]
          [:section#docs]
          [:section#gallery]
          [:div#scripts {:style {:display "none"}}
           ;; Codemirror scripts
           [:script {:src "resources/codemirror.min.js"}]
           [:script {:src "resources/clojure.min.js"}]
           [:script {:src "resources/matchbrackets.min.js"}]
           [:script {:src "resources/closebrackets.min.js"}]
           ;; HTMX scripts
           [:script {:src "resources/htmx.min.js"}]
           ;; sdfx render script
           [:script {:src "resources/render.js"}]]]]))))

(defn split-by [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (let [[xs ys] (split-with pred s)]
        (if (seq xs)
          (cons xs (split-by pred ys))
          (let [!pred (complement pred)
                skip (take-while !pred s)
                others (drop-while !pred s)
                [xs ys] (split-with pred others)]
            (cons (concat skip xs)
                  (split-by pred ys))))))))

(defn- split-path
  "split the list of points at any large jumps in the chain."
  [pts]
  (let [segments (partition 2 1 pts)
        lengths (mapv #(apply svg.u/distance %) segments)
        threshhold (* 6 (apply svg.u/average lengths))]
    (->> (map vector segments lengths)
         (mapv (fn [[segment l]] (when (< l threshhold) (first segment))))
         (split-by nil?)
         (filter seq)
         (mapv (fn [pts] (vec (remove nil? pts)))))))

(defn join-paths
  [& paths]
  (let [ms (map (comp #(select-keys % [:d]) second) paths)
        props (-> (last paths) second (dissoc :d))]
    [:path (merge
            (apply (partial merge-with str) ms)
            props)]))

;; Route Response Functions

(defn initial-response
  [_]
  {:status 200
   :body   (apply str (template))})

(defn- resource-response
  [{:keys [params]}]
  (let [file (:file params)
        ext  (last (str/split file #"\."))]
    {:status  200
     :headers {"Content-Type" (format "text/%s" (if (= ext "js") "javascript" ext))}
     :body    (slurp (io/resource file))}))

(defn- docs-response
  [_]
  (swap! show-docs not)
  {:status 200
   :body
   (str
    (h/html
        (when @show-docs
          (into
           [:div
            [:h3 "Docs"]]
           (for [{:keys [name ns arglists doc]} (map #(meta (second %))
                                                     (concat
                                                      (ns-publics 'sdfx.geom)
                                                      (ns-publics 'svg-clj.parametric)))]
             [:dl
              (into
               [:dt.mono-font]
               (for [arglist arglists]
                 [:div (format "(%s/%s %s)" (str ns) name (str/join " " arglist))]))
              [:dd (str doc)]])))))})

(defn- gallery-response
  [_]
  (swap! show-gallery not)
  {:status 200
   :body
   (str
    (h/html
        (when @show-gallery
          [:div
           [:h3 "Gallery"]
           (into
            [:div#gallery-container.f-row.flex-wrap:wrap]
            (for [{:keys [svg-data src-str]} (reverse @contour-history)]
              [:div.f-row
               svg-data
               [:pre [:code src-str]]]))])))})

(defn- settings-response
  [{:keys [body]}]
  (reset! render-settings (u/query-string->map (slurp body)))
  {:status 200
   :body   nil})

(defn- contours-response
  [{body :body}]
  (let [{contours "contours" sdf-src "sdfSrc"} (u/parse-body body)
        svg-data
        (let [poly-pts (remove nil? (mapcat second (json/parse-string contours) #_(sort-by rgb-str->hue (u/parse-body body))))]
          (->
           (svg
            (for [pts poly-pts]
              (let [pts   (map (fn [pt] [(+ (get pt "x")) (- (get pt "y"))]) pts)
                    paths (map path/polygon (split-path pts))]
                (-> (apply join-paths paths)
                    (tf/style {:fill   "black"
                               :stroke "white"})))))
           (tf/style {:style {:max-width "400px" :max-height "400px"}})))]
    (update-contour-history! {:svg-data svg-data
                              :src-str  sdf-src})
    {:status 200
     :body   (str (h/html svg-data))}))

(defn- save-response
  [{:keys [body]}]
  (let [output-dir (io/file "output/")]
    (when-not (.exists output-dir)
      (.mkdir output-dir)))
  (let [f         (fn [body]
                    (try (slurp body)
                         (catch Exception _e "")))
        [_ fname] (str/split (f body) #"=")
        fname     (-> (or fname "")
                      str/trim
                      (->> (format "output/%s")))]
                  (if (not= fname "output/")
                    (do
                      (spit fname (str (h/html (last @contour-history))))
                      {:status 200
                       :body   (format "Successfully saved as %s" fname)})
                    {:status 200
                     :body   "Filename required to save."})))

(defn- src-response
  [{:keys [query-string]}]
  (let [{:keys [sdf-src]} (u/query-string->map-reading-values query-string)
        {original-src-str :sdf-src} (u/query-string->map query-string)
        src               (str/trim (str sdf-src)) #_(format "(do %s)" (str/trim (str sdf-src)))
        maybe-f           (u/maybe-load-string src)
        f                 (if (fn? maybe-f) maybe-f (fn [_] "p"))]
    (when (fn? maybe-f)
      (reset! last-src-str (format "(do %s)" original-src-str) #_(if (= (first sdf-src) 'do)
                                 (apply str (rest sdf-src))
                                 (str sdf-src))))
    {:status 200
     :body   (str (h/html (compile-sdf f @render-settings)))}))

(def ^:private routes
  [{:path     "/resources/:file"
    :method   :get
    :response resource-response}
   {:path     "/"
    :method   :get
    :response initial-response}
   {:path   "/settings"
    :method :post
    :response settings-response}
   {:path   "/docs"
    :method :get
    :response docs-response}
   {:path   "/gallery"
    :method :get
    :response gallery-response}
   {:path   "/contours"
    :method :post
    :response contours-response}
   {:path     "/save"
    :method   :post
    :response save-response}
   {:path     "/src"
    :method   :get
    :response src-response}])

(defn- app
  []
  #(ruuter/route routes %))

(defonce ^:private server (atom nil))

;; https://github.com/prestancedesign/get-port/blob/main/src/prestancedesign/get_port.clj
(defn- get-available-port
  "Return a random available TCP port in allowed range (between 1024 and 65535) or a specified one"
  ([] (get-available-port 0))
  ([port]
   (with-open [socket (ServerSocket. port)]
     (.getLocalPort socket))))

(defn get-port
  "Get an available TCP port according to the supplied options.
  - A preferred port: (get-port {:port 3000})
  - A vector of preferred ports: (get-port {:port [3000 3004 3010]})
  - Use the `make-range` helper in case you need a port in a certain (inclusive) range: (get-port {:port (make-range 3000 3005)})
  No args return a random available port"
  ([] (get-available-port))
  ([opts]
   (loop [port (:port opts)]
     (let [result
           (try
             (get-available-port (if (number? port) port (first port)))
             (catch Exception e (instance? BindException (.getCause e))))]
       (or result (recur (if (number? port) 0 (next port))))))))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn serve!
  [& {:keys [port]}]
  (stop-server)
  (let [available-port (get-port {:port (concat [(or port 9876)] (range 8000 9000))})]
    (reset! server (srv/run-server (#'app) {:port available-port}))
    (println "Server started on Port: " available-port)))

(defn -main
  []
  (serve!))





(comment


  (defn- save!
    [svg-data]
    (let [fname (str "output/" (gensym "contours-") ".svg")]
      (spit fname (str (h/html svg-data)))))

  (defn- show-latest
    []
    (tools/cider-show (last @contour-history)))

  (defn- show-nth
    [n]
    (tools/cider-show (nth @contour-history n)))

  (defn- save-latest!
    []
    (save! (last @contour-history)))

  (defn- save-nth!
    [n]
    (save! (nth @contour-history n)))
  )
