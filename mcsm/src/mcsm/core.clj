(ns mcsm.core
  (:gen-class))

(defn dist 
"calculate the r=x^2 + y^2"
[x y]
(+(* x x) (* y y)))


(defn in-circle
  [n]
  (if (<= (dist (rand 1) (rand 1)) 1)
        (inc n)
        n))


(defn mc-pi
  "..."
  [n-iter]
  (loop [circle 0 iter 0]
    (if (< iter n-iter)
      (recur (inc iter) (in-circle circle))
     (float (*  4 (/ circle n-iter) )))))



(loop [i 0 s 0]
  (if (< i 10)
    (recur (inc i) (inc s))
    s))
;; [0 1 4 9 16 25 36 49 64 81]



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
