(ns vkan.core
  (:gen-class)
  (:use [clojure.core.match :only (match)])
  (:use [defun :only [defun]])
  (require [vkclj.core :as vkapi])
  (require [clojure.data.json :as json])
  (require [vkan.csv_convert :as csv]))

(def buffer (atom []))
(def token "20ae6c1b3a945bf492b985e23371ca925e7897979ce036491878edf123c0f33f5b2b245d912e6bb6f02b3")


(defn check_str_id [subj]
  (case (string? subj)
    false {:error "Id must be integer!"}
    true (case (= (vec (re-seq #"^[0-9]*$" subj)) [subj])
           false {:error "Id must be integer!"}
           true subj)))

(defn add_new_only [old new]
  (let [new_ids  (vec (map #(:uid %) new))]
    (let [filtered_old (vec (filter (fn [oldel] (every? #(not= % (:uid oldel)) new_ids))
                                    old))]
      (vec (concat filtered_old new)))))

(defun get_friends_lst
       ([(lst :guard vector?)] (get_friends_lst {:progress 0 :of (count lst) :res [] :rest lst}))
       ([{:res res :rest []}]   (println (str "got " (count res) " new users ... it will take some time to handle them"))
                                res)
       ([{:progress pg :of of :res res :rest lst}]
         (let [todo (vec (rest lst)) this (first lst)]
           (Thread/sleep 500)
           (println (str "progress " pg " of " of " , result " (count res)))
           (let [callres (vkapi/get_friends_uids {:uid this :access_token token})]
             (let [newres (match callres
                                 {:error _} res
                                 _ (vec (concat res callres )))]
               (match callres {:error error} (println error) _ :ok)
               (get_friends_lst {:progress (+ pg 1)
                                 :of of
                                 :res newres
                                 :rest todo}))))))
(defn add_friends_to_buffer []
  (let [ids (vec (map #(:uid %) @buffer))]
    (case (= 0 (count ids))
      true (println "Empty buffer")
      false (match (get_friends_lst ids)
                   {:error error} (println error)
                   [] (println "Got empty set ... ")
                   vect (match (vkapi/users_info {:uids vect :access_token token})
                               {:error err} (println err)
                               res (swap! buffer #(add_new_only % res)))))))

(defn menu_add_proc [command]
  (println "Enter id")
  (match (-> (read-line) (check_str_id) )
         {:error err} (println err)
         id (case command
              "1" (match (vkapi/users_info {:uids [(read-string id)] :access_token token})
                         {:error err} (println err)
                         [res] (swap! buffer #(add_new_only % [res]))
                         ans (println (str "Unexpected ans, ignore : " ans)))
              "2" (match (vkapi/get_group_members {:gid id :access_token token})
                         {:error err} (println err)
                         [] (println "Got empty set ... ")
                         vect (match (vkapi/users_info {:uids (vec (map read-string (map str vect))) :access_token token})
                                     {:error err} (println err)
                                     res (swap! buffer #(add_new_only % res)))))))

(defn menu_add []
  (println "1 - manual \n2 - from group \n3 - add friends")
  (match (read-line)
         "1" (menu_add_proc "1")
         "2" (menu_add_proc "2")
         "3" (add_friends_to_buffer)
         _   (println "Incorrect command")))

(defn filter_buffer [] (println "this is not avalible yet (((( "))
(defn import_buffer_csv []
  (spit "csv.csv" (csv/encode @buffer))
  (println "done, csv.csv overwrited"))
(defn import_buffer_json []
  (spit "json.txt" (json/write-str @buffer))
  (println "done, json.txt overwrited"))

(defn menu []
  (println "\nMenu : \n1 - add to buffer \n2 - filter buffer \n3 - export buffer to csv \n4 - export buffer to json")
  (match (read-line)
         "1" (menu_add)
         "2" (filter_buffer)
         "3" (import_buffer_csv)
         "4" (import_buffer_json)
         _ (println "Incorrect command"))
  (println "Buffer size now is " (count @buffer))
  (menu))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (menu))