(ns ts.data
    (:gen-class)
    (:require [clojure.java.io :as io]
              [clojure.string :as str]
              [tech.v3.dataset :as ds]
              [tech.v3.libs.poi]))

(def data (ds/->dataset "https://github.com/techascent/tech.ml.dataset/raw/master/test/data/ames-train.csv.gz"
                        {:column-whitelist ["SalePrice" "1stFlrSF" "2ndFlrSF"]
                         :n-record 15
                         :parser-fn :float32}))
(def data-2 (ds/->dataset "https://github.com/techascent/tech.ml.dataset/raw/master/test/data/file_example_XLS_1000.xls"))

(meta (data-2 "Id"))

(ds/brief data-2)
