(ns us.chouser.scratchapi
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.pprint :refer [print-table]])
  (:import [java.time Instant]
           [java.time.temporal ChronoUnit]))

(defn fetch-xtoken
  "Exchanges a scratchsessionsid cookie for the x-token needed by the API."
  [sessionsid]
  (let [conn (doto (.openConnection (java.net.URL. "https://scratch.mit.edu/session/"))
               (.setRequestProperty "Cookie" (str "scratchsessionsid=" sessionsid))
               (.setRequestProperty "X-Requested-With" "XMLHttpRequest")
               (.setRequestMethod "GET"))
        status (.getResponseCode conn)
        body   (slurp (or (.getErrorStream conn) (.getInputStream conn)))]
    (when-not (= 200 status)
      (throw (ex-info "fetch-xtoken failed" {:status status :body body})))
    (-> body
        (json/read-str :key-fn keyword)
        :user
        :token)))

(defn fetch-messages-page
  [{:keys [username sessionsid xtoken offset limit] :or {offset 0 limit 40} :as req}]
  (prn (select-keys req [:offset :limit]))
  (let [url (str "https://api.scratch.mit.edu/users/" username "/messages"
                 "?limit=" limit
                 "&offset=" offset
                 "&x-token=" xtoken)
        conn (doto (.openConnection (java.net.URL. url))
               (.setRequestProperty "Cookie" (str "scratchsessionsid=" sessionsid))
               (.setRequestProperty "Accept" "application/json")
               (.setRequestProperty "User-Agent" "Mozilla/5.0")
               (.setRequestMethod "GET"))
        status (.getResponseCode conn)
        body   (slurp (.getInputStream conn))]
    (when-not (= 200 status)
      (throw (ex-info "fetch-messages-page failed" {:url url :status status :body body})))
    {:req req :resp (json/read-str body :key-fn keyword)}))

(defn fetch-messages-until [oldest-date-str req]
  (->> (iteration fetch-messages-page
                  {:initk (assoc req :offset 0)
                   :kf #(when (neg? (compare oldest-date-str
                                             (:datetime_created (peek (:resp %)))))
                          (update (:req %) :offset + (count (:resp %))))
                   :vf :resp})
       (into [] cat)))

(defn update-messages-edn!
  "Fetches new messages since the latest already in messages.edn (if it exists)
   and prepends them to that file. Loads secrets and xtoken automatically."
  ([] (update-messages-edn! "messages.edn"))
  ([messages-file]
   (let [secrets    (read-string (slurp "secrets.edn"))
         xtoken     (fetch-xtoken (:sessionsid secrets))
         req        (assoc secrets :xtoken xtoken)
         existing   (if (.exists (java.io.File. messages-file))
                      (read-string (slurp messages-file))
                      [])
         ;; Use the latest (first) message's datetime as our cutoff,
         ;; or a fallback date that fetches everything.
         oldest-str (or (:datetime_created (first existing)) "1970-01-01T00:00:00.000Z")
         new-msgs   (fetch-messages-until oldest-str req)
         ;; Drop any overlap (messages already in the file)
         existing-ids (into #{} (map :id) existing)
         fresh-msgs   (remove #(existing-ids (:id %)) new-msgs)
         merged       (into [] cat [fresh-msgs existing])]
     (spit messages-file (pr-str merged))
     (println (str "Wrote " (count merged) " messages ("
                   (count fresh-msgs) " new) to " messages-file "."))
     merged)))

(defn summarize-remixes [messages]
  (-> messages
      (->> (filter #(= "remixproject" (:type %)))
           (group-by (juxt :parent_id :parent_title)))
      (update-vals
       (fn f1 [msgs]
         (-> msgs
             (->> (group-by :actor_username))
             (update-vals
              (fn f2 [ms]
                (->> ms
                     (mapv (fn f3 [m]
                             {:url (str "https://scratch.mit.edu/projects/"
                                        (:project_id m) "/editor/")
                              :datetime (:datetime_created m)}))
                     (sort-by :datetime)
                     vec))))))))

(defn print-remix-tables [usernames summary]
  (let [most-recent-datetime (fn [actor-map]
                               (->> actor-map vals (mapcat identity)
                                    (map :datetime) sort last))
        top-5 (->> summary
                   (sort-by (fn [[_ actor-map]] (most-recent-datetime actor-map)))
                   (take-last 10))]
    (doseq [[[parent-id parent-title] actor-map] top-5]
      (print (str "\n" parent-title " (" parent-id ")"))
      (print-table [:nick :url :datetime]
                   (for [[nick username] usernames
                         {:keys [url datetime]} (get actor-map username)]
                     {:nick nick
                      :datetime datetime
                      :url url})))))

(defn remixes-by-actor [usernames messages]
  (let [by-actor (->> messages
                      (filter #(= "remixproject" (:type %)))
                      (group-by :actor_username))]
    (doseq [[nick username] usernames]
      (let [remixes (->> (get by-actor username)
                         (sort-by :datetime_created))]
        (print "\n" nick)
        (if (empty? remixes)
          (println "\n--NONE--")
          (print-table [:parent_title :url :datetime]
                       (for [m remixes]
                         (merge m
                                {:url (str "https://scratch.mit.edu/projects/"
                                           (:project_id m) "/editor/")
                                 :datetime (:datetime_created m)}))))))))

(comment
  (def secrets (read-string (slurp "secrets.edn")))
  (def xtoken (fetch-xtoken (:sessionsid secrets)))

  (def m (fetch-messages-page (assoc secrets :xtoken xtoken)))
  (count m)

  (def m (fetch-messages-until "2026-01-15" (assoc secrets :xtoken xtoken)))
  (count m) ;=> 46

  (update-messages-edn!)

  (remixes-by-actor (:usernames secrets) (read-string (slurp "messages.edn")))

  (print-remix-tables (:usernames secrets)
                      (summarize-remixes (update-messages-edn!)))

  (print-remix-tables (:usernames secrets)
                      (summarize-remixes (read-string (slurp "messages.edn"))))
  )
