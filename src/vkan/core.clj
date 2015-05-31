(ns vkan.core
  (:gen-class)
  (:use [clojure.core.match :only (match)])
  (:use [defun :only [defun]])
  (require [vkclj.core :as vkapi])
  (require [clojure.data.json :as json])
  (require [vkan.csv_convert :as csv])
  (require [clj-yaml.core :as yaml]))

(def buffer (atom []))
(defn token [] (slurp "./token.txt"))


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


(defn append_to_friends_file [iam myfriends]
                        (->> (map #(str iam "," %) myfriends)
                             (clojure.string/join "\n")
                             ((fn [st] (str st "\n")))
                             ((fn [st] (spit "friends.txt" st :append true)))))

(defun get_friends_lst
       ([(lst :guard vector?)] (get_friends_lst {:progress 0 :of (count lst) :res [] :rest lst}))
       ([{:res res :rest []}]   (println (str "got " (count res) " new users ... it will take some time to handle them"))
                                res)
       ([{:progress pg :of of :res res :rest lst}]
         (let [todo (vec (rest lst)) this (first lst)]
           (Thread/sleep 500)
           (println (str "progress " pg " of " of " , result " (count res)))
           (let [callres (vkapi/get_friends_uids {:uid this :access_token (token)})]
             (let [newres (match callres
                                 {:error _} res
                                 _ (vec (concat res callres )))]
               (match callres {:error error} (println error) _ (append_to_friends_file this callres))
               (get_friends_lst {:progress (+ pg 1)
                                 :of of
                                 :res newres
                                 :rest todo}))))))
;; work with files
(defn safe-delete [file-path]
  (if (.exists (clojure.java.io/file file-path))
    (try
      (clojure.java.io/delete-file file-path)
      (catch Exception e (str "exception: " (.getMessage e))))
    false))
(defn get_config []
  (if (.exists (clojure.java.io/file "config.yml"))
    (try
      (let [res (->> (slurp "config.yml")
                     (yaml/parse-string)
                     (:filters))]
        (case (map? res)
          true res
          false {:error (str "error while parsing config, got " res)}))
      (catch Exception e {:error (str "error while parsing config" e)}))
    {:error "config.yml is not exist"}))


(defn add_friends_to_buffer []
  (safe-delete "friends.txt")
  (let [ids (vec (map #(:uid %) @buffer))]
    (case (= 0 (count ids))
      true (println "Empty buffer")
      false (match (get_friends_lst ids)
                   {:error error} (println error)
                   [] (println "Got empty set ... ")
                   vect (match (vkapi/users_info {:uids vect :access_token (token)})
                               {:error err} (println err)
                               res (swap! buffer #(add_new_only % res))))))
  (println "done, file friends.txt overwrited"))

(defn menu_add_proc [command]
  (println "Enter id")
  (match (-> (read-line) (check_str_id) )
         {:error err} (println err)
         id (case command
              "1" (match (vkapi/users_info {:uids [(read-string id)] :access_token (token)})
                         {:error err} (println err)
                         [res] (swap! buffer #(add_new_only % [res]))
                         ans (println (str "Unexpected ans, ignore : " ans)))
              "2" (match (vkapi/get_group_members {:gid id :access_token (token)})
                         {:error err} (println err)
                         [] (println "Got empty set ... ")
                         vect (match (vkapi/users_info {:uids (vec (map read-string (map str vect))) :access_token (token)})
                                     {:error err} (println err)
                                     res (swap! buffer #(add_new_only % res)))))))

(defn get_inner_friends []
  (safe-delete "friends.txt")
  (let [input (vec (map #(:uid %) @buffer))]
    (doseq [uid input]
      (println (str "getting inner friends for " uid))
      (Thread/sleep 500)
      (match (vkapi/get_friends_uids {:uid uid :access_token (token)})
             {:error error} (println error)
             []  (println (str "no friends for " uid))
             lst (->> (filter (fn [el] (some #(= (str el) (str %)) input))
                              lst)
                      (vec)
                      (append_to_friends_file uid))))
    (println "done, file friends.txt overwrited")))

(defn menu_add []
  (println "1 - manual \n2 - from group \n3 - add friends")
  (match (read-line)
         "1" (menu_add_proc "1")
         "2" (menu_add_proc "2")
         "3" (add_friends_to_buffer)
         _   (println "Incorrect command")))
(defn filter_buffer_proc [config]
  (let [some_keys (keys config)]
    (swap! buffer (fn [lst]
                    (filter (fn [el] (every? #(= (str (% el)) (str (% config))) some_keys)) lst)))))
(defn filter_buffer []
  (match (get_config)
         {:error error} (println error)
         config (filter_buffer_proc config)))
(defn import_buffer_csv []
  (spit "users.csv" (csv/encode @buffer) :encoding "Cp1251")
  (println "done, csv.csv overwrited"))
(defn import_buffer_json []
  (spit "users.txt" (json/write-str @buffer) :encoding "Cp1251")
  (println "done, json.txt overwrited"))

(defn menu []
  (println "\nMenu : \n1 - add to buffer \n2 - filter buffer \n3 - export buffer to csv \n4 - export buffer to json \n5 - get inner friends network for buffer")
  (match (read-line)
         "1" (menu_add)
         "2" (filter_buffer)
         "3" (import_buffer_csv)
         "4" (import_buffer_json)
         "5" (get_inner_friends)
         _ (println "Incorrect command"))
  (println "Buffer size now is " (count @buffer))
  (menu))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (menu))