(ns vkan.csv_convert
  (:gen-class)
  (:use [clojure.core.match :only (match)])
  (:use [defun :only [defun]])
  (:use clojure-csv.core))

(defun struct_degradation_process
       ([(lst :guard vector?)]
         (case (= 0 (count lst))
           true {:fin nil}
           false (struct_degradation_process (first lst))))
       ([(lst :guard list?)]
         (case (= 0 (count lst))
           true {:fin nil}
           false (struct_degradation_process (first lst))))
       ([(some_map :guard map?)]
         (let [reskeys (vec (keys some_map))]
           (case (= 0 (count reskeys))
             true {:fin nil}
             false {:keys reskeys :map some_map})))
       ([some_else] {:fin some_else}))

(defn generate_key [main sub]
  (->> (map #(case (keyword? %) true (name %) false (str %)) [main sub])
       (clojure.string/join "_")
       (keyword)))

(defn plain_map_proc [resmap key some_map]
  (match (struct_degradation_process (key some_map))
         {:fin fin} (assoc resmap key fin)
         {:keys some_keys :map other_map}
                (reduce #(assoc %1 (generate_key key %2) (%2 other_map))
                        resmap
                        some_keys)))

(defn plain_map [some_map]
  (let [init_keys (keys some_map)]
    (let [result (reduce #(plain_map_proc %1 %2 some_map) {} init_keys)]
      (case (every? #(not (map? %)) (vals result))
        true result
        false (plain_map result)))))

(defn to_string [el]
  (case (keyword? el)
    true (name el)
    false (str el)))

(defn encode [lst]
  (let [plain (map plain_map lst)]
    (let [my_keys (keys (first plain))]
      (->> (concat [my_keys] (map (fn [el] (map #(% el) my_keys)) plain))
           (map #(map to_string %))
           ((fn [el] (write-csv el :force-quote true)))))))