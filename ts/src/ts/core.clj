(ns ts.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.string :as str]
            [tech.v3.dataset :as ds]))

;;; https://github.com/techascent/tech.ml.dataset
;;; https://techascent.github.io/tech.ml.dataset/walkthrough.html


(def csv-data (ds/->dataset "city_data.csv"))

(ds/head csv-data)
(csv-data "Milan")


(defn clean-trip
  "..."
  [trip]
  (let [trip (remove #(= % "Row-name") trip)]
    (zipmap (range 1 (+ (count trip) 1 )) trip)))



(defn helper
  [d-matrix s]
  (let [s (first (sort s))]
    (nth (d-matrix (val s)) (+ (key s) 1))))

(defn objective
  " calculate the objective value"
  [trip d-matrix]
  (let [the-seq (seq trip)]
    (loop [energy []
           s the-seq]
      (if (seq s)
        (recur (conj energy (helper d-matrix s) )
                   (rest s))
        (/ (reduce + energy) 1000.0)))))





(comment
  (def trip (remove #(= "Row-name" %) (ds/column-names csv-data)))
  (def map-trip (zipmap (range 1 25) trip))
  (second (csv-data (val (first map-trip))))
  (defn my-count
  [col]
  (let [the-seq (seq col)]
    (loop [n 0 s the-seq]
      (if (seq s)
        (recur (inc n) (rest s))
        n))))


  )



#_(defn read-csv
  [file-path]
  (with-open [file (io/reader file-path)]
    (doall (csv/read-csv file))))

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map keyword) ;; Drop if you want string keys instead
            repeat)
       (rest csv-data)))
  #_(repeat (map keyword (first csv-data zipmap)))



;;(def data (read-csv "city_data.csv"))
;;(def csv-data (maps->tabular ))


(def csv-data (csv-data->maps (read-csv "city_data.csv")))

(defn temp
	"just genrate a vector of doubles
	inputs:
		initN: initial temperature
		finN: final temperature
		rate: a negative value for decrase rate
		in temperature
	output:
		a vector of doubles"

	[initN finN rate]
	(range initN finN rate))







(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (PRINTLN "HELLO, WORLD!"))
