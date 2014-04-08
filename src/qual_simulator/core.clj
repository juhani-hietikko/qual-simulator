(ns qual-simulator.core
  (:gen-class)
  (:require [clojure.core.async :refer [chan >!! <!! close!]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn mc-simulate [times simulate-once output-channel]
  (future
    (dotimes [_ times]
      (>!! output-channel (simulate-once)))
    (close! output-channel)))

(defn random-match-result [match-with-probabilities]
  (let [rnd-percent (inc (rand-int 100))
        home-team (first match-with-probabilities)
        away-team (nth match-with-probabilities 1)
        home-win-prob (nth match-with-probabilities 2)
        draw-prob (nth match-with-probabilities 3)
        away-win-prob (nth match-with-probabilities 4)]
    (cond
      (not= 100 (+ home-win-prob draw-prob away-win-prob))
      (throw (IllegalArgumentException. "Probabilities don't sum to 100%"))
      (<= rnd-percent home-win-prob)
      {:home home-team :home-points 3 :away away-team :away-points 0}
      (<= rnd-percent (+ home-win-prob draw-prob))
      {:home home-team :home-points 1 :away away-team :away-points 1}
      :else
      {:home home-team :home-points 0 :away away-team :away-points 3})))

(defn simulate-match [standings-before match-with-probabilities]
  (let [match-result (random-match-result match-with-probabilities)
        home-team (:home match-result)
        away-team (:away match-result)
        home-team-points-before (home-team standings-before)
        away-team-points-before (away-team standings-before)]
    (-> standings-before
        (assoc home-team (+ home-team-points-before (:home-points match-result)))
        (assoc away-team (+ away-team-points-before (:away-points match-result))))))

(defn simulate-qual-once [initial-standings matches-with-probabilities]
  (let [standings-with-points (reduce simulate-match 
                                      initial-standings 
                                      matches-with-probabilities)
        sorted-standings (sort-by last > standings-with-points)]
    (map first sorted-standings)))

(defn print-results [result-count results-channel]
  (future (dotimes [_ result-count] 
            (println (<!! results-channel)))))

(defn accumulate-to-distrib [acc current-result]
  (loop [current-acc acc
         remaining-teams current-result
         current-position 1]
    (if (empty? remaining-teams)
      current-acc
      (let [this-team (first remaining-teams)
            teams-tail (rest remaining-teams)
            new-position-val (inc (get-in current-acc [this-team current-position]))]
        (recur (assoc-in current-acc [this-team current-position] new-position-val)
               teams-tail
               (inc current-position))))))

(defn reduce-results [result-count results-channel]
  (let [results-seq (for [x (range 0 result-count)]
                      (<!! results-channel))
        distrib-seed {:greece {1 0 2 0 3 0 4 0 5 0 6 0}
                      :hungary {1 0 2 0 3 0 4 0 5 0 6 0}
                      :romania {1 0 2 0 3 0 4 0 5 0 6 0}
                      :finland {1 0 2 0 3 0 4 0 5 0 6 0}
                      :northern-ireland {1 0 2 0 3 0 4 0 5 0 6 0}
                      :faroe-islands {1 0 2 0 3 0 4 0 5 0 6 0}}
        distrib (reduce accumulate-to-distrib distrib-seed results-seq)]
    distrib))

(defn mc-simulate-qual [initial-standings matches-with-probabilities]
  (let [results-channel (chan 2)
        times-to-run 10000]
    (mc-simulate times-to-run 
                 (fn [] (simulate-qual-once initial-standings matches-with-probabilities))
                 results-channel)
    (reduce-results times-to-run results-channel)))

(def standings {:greece 0
                :hungary 0
                :romania 0
                :finland 0
                :northern-ireland 0
                :faroe-islands 0})

(def matches [[:hungary :northern-ireland 70 20 10]
              [:faroe-islands :finland 70 20 10]
              [:greece :romania 70 20 10]
              [:romania :hungary 70 20 10]
              [:finland :greece 70 20 10]
              [:northern-ireland :faroe-islands 70 20 10]
              [:faroe-islands :hungary 70 20 10]
              [:finland :romania 70 20 10]
              [:greece :northern-ireland 70 20 10]
              [:greece :faroe-islands 70 20 10]
              [:hungary :finland 70 20 10]
              [:romania :northern-ireland 70 20 10]
              [:northern-ireland :finland 70 20 10]
              [:romania :faroe-islands 70 20 10]
              [:hungary :greece 70 20 10]
              [:finland :hungary 70 20 10]
              [:northern-ireland :romania 70 20 10]
              [:faroe-islands :greece 70 20 10]
              [:faroe-islands :northern-ireland 70 20 10]
              [:greece :finland 70 20 10]
              [:hungary :romania 70 20 10]
              [:finland :faroe-islands 70 20 10]
              [:northern-ireland :hungary 70 20 10]
              [:romania :greece 70 20 10]
              [:hungary :faroe-islands 70 20 10]
              [:northern-ireland :greece 70 20 10]
              [:romania :finland 70 20 10]
              [:faroe-islands :romania 70 20 10]
              [:finland :northern-ireland 70 20 10]
              [:greece :hungary 70 20 10]])

(def example-sim-result [:greece 
                         :finland 
                         :romania 
                         :hungary 
                         :northern-ireland 
                         :faroe-islands])
