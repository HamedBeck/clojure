(ns mcs.core
  (:gen-class))

(defn dist
  "calculate the distance r"
  [x y]
  (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))

(defn in-circle
  [n]
  (cond (<= (dist (rand 1) (rand 1)) 1)
        (inc n)
        :else
        n))

(defn mc-pi
  "..."
  [n-iter]
  (loop [circle 0
        iter 0]
    (if (<= iter n-iter)
      (recur  (in-circle circle)  (inc iter))
     (float (* 4 (/ circle n-iter))))))



(defn my-function
  "Function to be integrated"
  [x]
  (* (Math/pow x 4) (Math/exp (* -1 x))))

(defn my-function
  "..."
  [x]
  (Math/pow x 2))


(defn mc-intgeral
  "..."
  [low-bound up-bound n-iter]
  (loop [tot-sum 0
         iter 0]
    (if (< iter n-iter)
      (let [rand-num (+ low-bound (* (rand 1) (- up-bound low-bound)))
            function-val (my-function rand-num)]
        (recur (+ tot-sum function-val)(inc iter)))
      (/ (* (- up-bound low-bound) tot-sum) n-iter ))))


(defn qrt-mc-pi
    [n-iter]
    (let [helper (fn [circle q]
                   (when (in-circle)
                     (println circle q)
                     (recur (inc circle) (dec q))))]
      (helper 0 33)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
