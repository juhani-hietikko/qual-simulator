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

(defn simulate-qual-once [initial-standings matches-with-probabilities]
  1)

(defn print-results [times results-channel]
  (future (dotimes [_ times] 
    (println (<!! results-channel)))))

(defn mc-simulate-qual [initial-standings matches-with-probabilities]
  (let [results-channel (chan 2)]
    (mc-simulate 6 
                 (fn [] (simulate-qual-once initial-standings matches-with-probabilities))
                 results-channel)
    @(print-results 6 results-channel)))
