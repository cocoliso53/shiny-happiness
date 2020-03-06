(ns clojure-noob.core
  (:gen-class)
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

;; Math

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "I am a little teapot!"))
(println "Cleanliness is the next godliness")

(defn discrete-metric [x y] (if (= x y) 0 1))
(defn euclidean-metric [x y] (Math/abs (- x y)))


(defn metric [f x y]
  "f = (f_1,...f_n), x = (x_1,...,x_n), y = (y_1,...,y_n)
   metric(f,x,y) = sum( f_i(x_i,y_i))  "
  (reduce + (map #(%1 %2 %3) f x y)))


(defn knn [f p_0 v]
  "f = (f_1,...,f_n), p_0 = (p_1,...p_n), v = [class_i [feature_i]] with
   p_0, feature_i in X, class_i in G.  knn(f,p_0,v) = (metric(f,p_0,feature_0),...,metric(f,p_0,feature_n)"
  (map #(vector (first %) (metric f p_0 (nth % 1))) v))

(defn neigh [res n]
  "res comes from knn-complete n is neighborhood size"
  (filter #(>= n (nth %1 1)) res))

(defn knn-neighbors [f p t n]
  (first (split-at n (sort-by last < (knn f p t)))))
  

(defn knn-neighborhood [f p t]
  "f is a vector of metrics, p is a point in X and t is like {[c [v]]}_i i in I, where c is class and v in X"
  (neigh (map #(vector (first %) (metric f p (nth % 1))) t) 0))

(defn predicted-class [ceros]
  "ceros is the result of knn-complete"
  (map #(key (apply max-key val (frequencies (map first %)))) ceros))

;;(def metrica [euclidean-metric discrete-metric discrete-metric euclidean-metric discrete-metric discrete-metric euclidean-metric euclidean-metric])
;;(def features [":income" ":age" ":workclass" ":occupation" ":education_num" ":marital_status" ":sex" ":hours_per_week" ":capital_gain"])
;; (map first (neigh dd 0))
;; (key (apply max-key val (frequencies vecinos))) 
;; (map #(key (apply max-key val (frequencies (map first %)))) ceros) ceros viene knn-complete
;; Data processing

(def hongos-path "./resources/mushrooms.csv")

(defn demo-reader [path]
  (with-open [reader (io/reader path)]
    (doall (csv/read-csv reader))))


(defn make-table [data]
  "data is the result of applying demo-reader to some csv file where the
   first row are the name of the categories, the result is row turned into
   hashmaps which allows indexation, additionally it adds a num column"
  (map #(zipmap (first data) %1) (rest data)))

(defn set-index [data]
  (zipmap (range) data))


(defn select-values [map ks]
  (reduce #(conj %1 (map %2)) [] ks))

(defn properties-interest [coll ks]
  "Selects only the features of interest (ks) for each element in coll in the spercified order, returns a list of vectors.
 Should add 'num' as the first element in ks for now"
  (map #(select-values %1 ks) coll))


(defn prueba1 [t]
  "t is a hashmap with the key being a number and the value a list of values from important features including class, we separeta keys
   from values so we can operate with them better"
  (let [k (keys t)
        clase (map first (vals t))
        features (map rest (vals t))]
    (zipmap k features)))

(defn sort-result [result] (reverse 
  (into (sorted-map-by (fn [key1 key2]
                          (compare [(get result key2) key2]
                                  [(get result key1) key1]))) result)))

(defn prueba2 [t]
  "Almost the same as prueba1 but returns {k [class (features)],...}"
  (let [k (keys t)
        clase (map first (vals t))
        features (map rest (vals t))]
    (zipmap k (map vector clase features))))

(defn separar [coll]
  "coll es resultado de properties-interest"
  (let [clase (map first coll)
        features (map rest coll)]
    (map vector clase features)))

(def pre-split (comp shuffle separar properties-interest))
