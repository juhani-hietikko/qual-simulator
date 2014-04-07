(ns qual-simulator.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn mc-simulate [times simulate-once combine-results]
  (loop [times-left times
         results-acc nil]
    (if (= 0 times-left)
      results-acc
      (recur (dec times-left) (combine-results results-acc (simulate-once))))))

(defn simulate-qual-once [initial-standings matches-with-probabilities]
  nil)

(defn combine-simulation-result-to-others [acc-results result]
  nil)

(defn mc-simulate-qual [initial-standings matches-with-probabilities]
  (mc-simulate 10 
               (fn [] (simulate-qual-once initial-standings matches-with-probabilities))
               combine-simulation-result-to-others))


(defn sim-foo []
  (rand-int 10))

(defn comb-foo [acc new-res]
  (if (nil? acc)
    [new-res]
    (conj acc new-res)))
