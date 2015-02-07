(ns sleeping-barber
  (:require [midje.sweet :refer :all]))

(defn customer-arrival-intervals [min max length]
  (->> (repeatedly #(+ min (rand-int (- max min))))
       (reductions (fn [[sum _] n] [(+ sum n) n]) [0 0])
       rest
       (take-while (fn [[sum _]] (<= sum length)))
       (map second)))

(defn sit-or-leave [waiting-room customer]
  (if (< (count waiting-room) 3)
    (conj waiting-room customer)
    waiting-room))

(defn arrivals [simulation-length waiting-room]
  (future
    (doall
      (for [[interval customer-number] (map vector
                                            (customer-arrival-intervals 10 30 simulation-length)
                                            (range))]
        (do
          (Thread/sleep interval)
          (dosync
            (alter waiting-room sit-or-leave customer-number)))))))

(defn service-customer [waiting-room customers-serviced]
  (dosync (alter waiting-room rest)
          (alter customers-serviced inc))
  (Thread/sleep 20))

(defn hairdresser [simulation-length waiting-room]
  (let [end                (+ (System/currentTimeMillis) simulation-length)
        customers-serviced (ref 0)]
    (do
      (while (<= (System/currentTimeMillis) end)
          (if (not (empty? @waiting-room))
            (service-customer waiting-room customers-serviced)))
      @customers-serviced)))

(defn start-simulation [simulation-length]
  (let [waiting-room (ref [])]
    (arrivals simulation-length waiting-room)
    (hairdresser simulation-length waiting-room)))

