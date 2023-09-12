(ns jansuran03.datamerge.core)

(defn transform-map
  "Goes through all map entries and returns a new map such that the keys
  correspond to the original keys and the values are non-nil results of
  applying the keyfn to the original value.
  Example use:
  ```clojure
  (let [dict #:translate{:home     #:lang{:en \"Home\"
                                          :de \"Startseite\"}
                         :projects #:lang{:en \"Projects\"
                                          :de \"Projekte\"}
                         :todo     #:lang{:de \"lang `en` not provided\"}}]
    (transform-map :lang/en dict))
  user=>  #:translate{:home     #:lang{:en \"Home\"}
                      :projects #:lang{:en \"Projects\"}}
   ```
  If no input collection is supplied, instead returns a transducer that will
  go through the next input map and reduce the mapped kv-pairs as vectors into
  the current intermediate result."
  ([keyfn]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (reduce-kv (fn [m k v]
                     (if-some [v (keyfn v)]
                       (rf m [k v])
                       m))
                   result
                   input)))))
  ([keyfn coll]
   (into {}
         (keep (fn [[k v]]
                 (some->> (keyfn v) (vector k))))
         coll)))

(defn transform-maps
  "Given key function and a list of maps, performs transformation for each of
  the maps and merges them together. See [[transform-map]] for details."
  ([keyfn maps]
   (into {}
         (transform-map keyfn)
         maps)))

(def navbar-dict #:translate{:home     #:lang{:cs "Úvod"
                                              :en "Home"}
                             :profile  #:lang{:cs "Profil"
                                              :en "Profile"}
                             :projects #:lang{:cs "Projekty"
                                              :en "Projects"}})
(def lang-dict #:translate{:czech     #:lang{:cs "čeština - rodný jazyk"
                                             :en "Czech - native language"}
                           :english   #:lang{:cs "angličtina - plynulá"
                                             :en "English - fluent"}
                           :german    #:lang{:cs "němčina - plynulá (C1)"
                                             :en "German - fluent (C1)"}
                           :languages #:lang{:cs "Jazyky"
                                             :en "Languages"}})
