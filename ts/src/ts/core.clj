
(ns ts.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.string :as str]
            [tech.v3.dataset :as ds]))

;;; https://github.com/techascent/tech.ml.dataset
;;; https://techascent.github.io/tech.ml.dataset/walkthrough.html


(def csv-data (ds/->dataset "city_data.csv"))

#_(ds/head csv-data)
#_(csv-data "Milan")


#_(defn clean-trip
  "create a map containing the cities"
  [trip]
  (let [trip (remove #(= % "Row-name") trip)]
    (zipmap (range 1 (+ (count trip) 1 )) trip)))

;; generate secheduling rates
(defn temp
	"just genrate a vector of doubles
	inputs:
		N: number of temperature points
                iniT : initial temerature
		rate: decrase rate in temperature
	output:
		a list of doubles"

	[N iniT rate]
	 (for [i (range 1 N)] (/ iniT (Math/log (+ rate i)))))


(defn tempt
  "..."
  [iniT finT rate]
  (range iniT finT rate))



(defn init-trip
  "Generate the first random configuration"
  [csv-data]
  (let [cities (remove #(= "Row-name" %) (ds/column-names csv-data))
        trip (zipmap (range 1 (inc (count cities))) cities)]
    #_(zipmap (range 1 25) trip)
    (conj trip {(inc (count trip)) (val (first (sort trip)))})))


(defn update-trip
  "update the trip map by swap of two random items"
  [trip]
  (let [i (+ 2 (rand-int (- (count trip) 2)))
        j (+ 2 (rand-int (- (count trip) 2)))]
    (-> trip (assoc i (trip j)) (assoc j (trip i)))))



(defn pull-distance
  "pulls distance value from matrix"
  [d-matrix s]
  (let [p (first (sort s))]
    (nth (d-matrix (val p)) (key p))))

;;;; does not calculate dist. from last city to start

(defn objective
  " calculate the objective value"
  [trip d-matrix]
  (loop [energy []
         i 0]
    (if (< i (- (count trip) 2))
        (recur (conj energy (pull-distance d-matrix (filter #(> (key %) i) trip)))
               (inc i))
        (/ (reduce + energy) 1000.0))))



(defn acceptance
  "compare to state and returen the best"
  [c n t]
  (let [cE (objective c csv-data)
        nE (objective n csv-data)
        delta (- nE cE)]
    (cond
      (pos? delta) n
      (and (neg? delta)
           (>  (Math/exp (/ (* delta -1) t)) (+ (rand 0.99) 0.01) )) n
      :else c)))



(defn metropolis
  "..."
  [csv-data temp]
  (loop [best (init-trip csv-data)
         E-list []
         i 0]
    #_(println best)
    (if (< i (count temp))
      (recur  (acceptance best (update-trip best) (nth temp i))
              (conj E-list (objective best csv-data))
              (inc i))  {:energy-list E-list :last-configuration best})))




(defn runner
  [csv-data iniT finT rate]
  (let [temp (tempt iniT finT rate)]
    (println "TEMP " (count temp))
    (metropolis csv-data temp)))




#_(insp/inspect-table {:energy-list nest})



;;(def data (read-csv "city_data.csv"))
;;(def csv-data (maps->tabular ))


#_(def csv-data (csv-data->maps (read-csv "city_data.csv")))





#_(-> {:x (range 99)
     :y (repeatedly 99 rand)}
    tc/dataset
    viz/data
    (viz/type :line)
    (viz/viz))

(defn -main
  "I don't do a whole lot ... yet."
  [&args]
  (println "hello"))
