(ns docnudb.backend.db
  (:require [datalevin.core :as d]))

(defmacro let [[b v & more-bindings] & body]
  (cond (nil? b)
        `(do ~@body)
        (= b :escape)
        `(or ~v
             (elet ~more-bindings ~@body))
        :else
        `(let [~b ~v]
           (elet ~more-bindings ~@body))))

#_(let [x (determine-x)]
    (if (something-wrong-with x)
      "early return, because x is wrong"
      (let [y (determine-y x)]
        (if (something-wrong-with y)
          "early return, because y is wron")
        (let [z (determine-z y)]
          (do-something-with z)))))

#_(let [x (determine-x)            :escape (when (something-wrong-with x)
                                             "early return, because x is wrong")
        y (determine-y x)          :escape (when (something-wrong-with y)
                                             "early return, because y is wron")
        z (determine-z y)]
    (do-something-with z))

#_(let [x (determine-x)
        :escape (when (something-wrong-with x) "early return, because x is wrong")
        y (determine-y x)
        :escape (when (something-wrong-with y) "early return, because y is wron")
        z (determine-z y)]
    (do-something-with z))

; What I often did:
#_(let [x (determine-x)
        y (determine-y x)
        z (determine-z y)]
    (cond (something-wrong-with x)
          "early return, because x is wrong"
          (something-wrong-with y)
          "early return, because y is wrong"
          :no-error
          (do-something-with z)))
; But there is a principal design flaw with this. If something is wrong with x, I still calculate y and z,
; although not needed. That is often wrong, because I should not propagate x once I know it is invalid.
; Otherwise I need extra code in determine-y and determine-z, that makes the implementation tolerant against
; an incorrect input.
; My escape in let bindings feels like a recourse to imperative programming. That is true. But this is often the
; nature of a problem. That is why let has a sequential nature and why we have the threading macros. But sometimes
; a problem needs a way to bail out.
;
; Practical advantages of this:
;  * The exceptional cases do not distract, when reading the code.
;  * A sequence stays a sequence.



(def schema
  {:type {:db/cardinality :db.cardinality/one
          :db/valueType :db.type/keyword}
             ;;; :docdb/document
   :document/number {:db/cardinality :db.cardinality/one
                     :db/valueType :db.type/string}
   :document/title {:db/cardinality :db.cardinality/one
                    :db/valueType :db.type/string}
             ;;; :docdb/release
   :release/document {:db/cardinality :db.cardinality/one
                      :db/valueType :db.type/ref}
   :release/issue {:db/cardinality :db.cardinality/one
                   :db/valueType :db.type/string}
   :release/docrefs {:db/cardinality :db.cardinality/many
                     :db/valueType :db.type/ref}
             ;;; :docdb/docref
   :docref/pos {:db/cardinality :db.cardinality/one
                :db/valueType :db.type/long}
   :docref/name {:db/cardinality :db.cardinality/one
                 :db/valueType :db.type/string}
   :docref/release {:db/cardinality :db.cardinality/one
                    :db/valueType :db.type/ref}
   :docref/external-release {:db/cardinality :db.cardinality/one
                             :db/valueType :db.type/string}})

(def conn (d/get-conn "docdb" schema))

(comment

  (d/transact
   conn
   [{:type :docdb/document :db/id "spm"
     :document/number "F-J-SPM-013"
     :document/title "Software Programmers Manual"}
    {:type :docdb/release
     :release/document "spm"
     :release/issue "C"
     :release/docrefs ["sdp-b-ref" "scmp-1-ref" "do178c-ref"]}
    {:type :docdb/docref :db/id "sdp-b-ref"
     :docref/pos 0
     :docref/release "sdp-b"}
    {:type :docdb/docref :db/id "scmp-1-ref"
     :docref/pos 1
     :docref/release "scmp-1"}
    {:type :docdb/docref :db/id "do178c-ref"
     :docref/pos 2
     :docref/external-release "DO-178C"}
    {:type :docdb/document :db/id "sdp"
     :document/number "F-J-SDP-027"
     :document/title "Software Development Plan"}
    {:type :docdb/release :db/id "sdp-b"
     :release/document "sdp"
     :release/issue "B"}
    {:type :docdb/document :db/id "scmp"
     :document/number "F-J-SCP-027"
     :document/title "Software Configuration Management Plan"}
    {:type :docdb/release :db/id "scmp-1"
     :release/document "scmp"
     :release/issue "1"}])
  "F-J-SPM-013 Software Programmers Manual
     C -> F-J-SDP-027 Software Development Plan B
          F-J-SCP-027 Software Configuration Management Plan 1
          'DO-178C'
   F-J-SDP-027 Software Development Plan
     B
   F-J-SCP-027 Software Configuration Management Plan
     1"

  (d/close conn))

(defn- query-documents []
  (map
   (fn [[doc-number title]]
     {:doc-number doc-number :title title})
   (d/q '[:find ?doc-number ?title
          :where
          [?document-ent ?type :docdb/document]
          [?document-ent :document/number ?doc-number]
          [?document-ent :document/title ?title]]
        (d/db conn))))

(comment
  (query-documents))

(defn- query-document-ent [doc-number]
  (d/q '[:find ?document-ent
         :in $ ?doc-number
         :where
         [?document-ent ?type :docdb/document]
         [?document-ent :document/number ?doc-number]]
       (d/db conn) doc-number))

(comment
  (query-document-ent "F-J-SPM-013"))

(defn- tx-document [doc-number title]
  [{:type :docdb/document
    :document/number doc-number
    :document/title title}])

(defn- query-releases [document-ent]
  (map (fn [[issue]] {:issue issue})
       (d/q '[:find ?issue
              :in $ ?document-ent
              :where
              [?release-ent :type :docdb/release]
              [?release-ent :release/document ?document-ent]
              [?release-ent :release/issue ?issue]]
            (d/db conn) document-ent)))

(comment
  (->> (query-document-ent "F-J-SPM-013")
       first first
       query-releases))

(defn- query-release-ent [document-ent issue]
  (d/q '[:find ?release-ent
         :in $ ?document-ent ?issue
         :where
         [?release-ent :type :docdb/release]
         [?release-ent :release/document ?document-ent]
         [?release-ent :release/issue ?issue]]
       (d/db conn) document-ent issue))

(comment
  (let [spm-ent (-> "F-J-SPM-013" query-document-ent first first)]
    (query-release-ent spm-ent "C")))

(defn- tx-release [document-ent issue]
  [{:type :docdb/release
    :release/document document-ent
    :release/issue issue}])

(defn- query-release [release-ent]
  (let [[docnum docname issue]
        (first (d/q '[:find ?doc-number ?doc-title ?issue
                      :in $ ?release-ent
                      :where
                      [?release-ent :release/document ?doc-ent]
                      [?release-ent :release/issue ?issue]
                      [?doc-ent :document/title ?doc-title]
                      [?doc-ent :document/number ?doc-number]]
                    (d/db conn) release-ent))]
    {:title docname :number docnum :issue issue}))

(comment
  (-> (query-document-ent "F-J-SPM-013")
      first first
      (query-release-ent "C")
      first first
      query-release))

(defn- query-docrefs-ents
  ([release-ent]
   (d/q '[:find ?pos ?docref-ent
          :in $ ?release-ent
          :where
          [?release-ent :release/docrefs ?docref-ent]
          [?docref-ent :docref/pos ?pos]]
        (d/db conn) release-ent))
  ([release-ent pos]
   (d/q '[:find ?docref-ent
          :in $ ?release-ent ?pos
          :where
          [?release-ent :release/docrefs ?docref-ent]
          [?docref-ent :docref/pos ?pos]]
        (d/db conn) release-ent pos)))

(comment
  (-> (query-document-ent "F-J-SPM-013")
      first first
      (query-release-ent "C")
      first first
      query-docrefs-ents)
  (-> (query-document-ent "F-J-SPM-013")
      first first
      (query-release-ent "C")
      first first
      (query-docrefs-ents 1)))

(defn- query-internal-docrefs [release-ent]
  (map (fn [[pos docref-release-ent]]
         (assoc (query-release docref-release-ent)
                :pos pos))
       (d/q '[:find ?docref-pos ?docref-release-ent
              :in $ ?release-ent
              :where
              [?release-ent :release/docrefs ?docref-ent]
              [?docref-ent :docref/pos ?docref-pos]
              [?docref-ent :docref/release ?docref-release-ent]]
            (d/db conn) release-ent)))

(comment
  (-> (query-document-ent "F-J-SPM-013")
      first first
      (query-release-ent "C")
      first first
      query-internal-docrefs))

(defn- query-external-docrefs [release-ent]
  (map (fn [[pos external-release]]
         {:pos pos
          :external-release external-release})
       (d/q '[:find ?docref-pos ?docref-external-release
              :in $ ?release-ent
              :where
              [?release-ent :release/docrefs ?docref-ent]
              [?docref-ent :docref/pos ?docref-pos]
              [?docref-ent :docref/external-release ?docref-external-release]]
            (d/db conn) release-ent)))

(comment
  (-> (query-document-ent "F-J-SPM-013")
      first first
      (query-release-ent "C")
      first first
      query-external-docrefs))

(defn- query-docrefs-to [release-ent]
  (d/q '[:find ?source-ent ?pos
         :in $ ?target-release-ent
         :where
         [?source-ent :type :docdb/release]
         [?source-ent :release/docrefs ?docref-ent]
         [?docref-ent :type :docdb/docref]
         [?docref-ent :docref/pos ?pos]
         [?docref-ent :docref/release ?target-release-ent]]
       (d/db conn) release-ent))

(comment
  (-> (query-document-ent "F-J-SCP-027")
      first first
      (query-release-ent "1")
      first first
      (query-docrefs-to)
      first first
      query-release))

(defn tx-remove-docref [docref-ent]
  [[:db/retractEntity docref-ent]])

(defn tx-update-docref-pos [docref-ent pos]
  [[:db/add docref-ent :docref/pos pos]])

(defn tx-make-docref-external [docref-ent external-release]
  [[:db/retract docref-ent :docref/release]
   [:db/add docref-ent :docref/external-release external-release]])

;;;; The API

(defn list-documents []
  {:result
   (sort-by :doc-number (query-documents))})

(comment
  (list-documents))

(defn add-document [number title]
  (let [:escape (when (not (empty? (query-document-ent number)))
                  {:error :already-exists})
        tx (tx-document number title)]
    (d/transact conn tx)
    {:result :success}))

(comment
  (add-document "F-J-ACsC-0dd74" "Accomplishment Summary")
  (list-documents))

(defn list-releases [document-number]
  (let [document-id (-> document-number query-document-ent first first)
                                                            :escape (when (not document-id) 
                                                                      {:error :non-existing-document})]
    {:result (->> (query-releases document-id)
                  (sort-by :issue))}))

(comment
  (list-releases "F-J-SPM-013"))

(defn- get-release [document-number issue]
  (let [document-id (-> document-number query-document-ent first first)
                                                            :escape (when (not document-id) 
                                                                      [nil nil :non-existing-document])
        release-id (-> (query-release-ent document-id issue) first first)
                                                            :escape (when (not release-id)  
                                                                      [document-id nil :non-existing-issue])]
    [document-id release-id nil]))

(defn add-release [document-number issue]
  (let [[document-id release-id error] (get-release document-number issue)
                                                            :escape (when (not document-id)
                                                                      {:error error})
                                                            :escape (when release-id
                                                                      {:error :alreay-existing-issue})]
    (d/transact conn (tx-release document-id issue))
    {:result :success}))

(comment
  (add-release "F-J-SPM-013" "V")
  (list-releases "F-J-SPM-013"))

(defn list-docrefs [document-number issue]
  (let [[_ release-id error] (get-release document-number issue)
                                                            :escape (when error  
                                                                      {:error error})]
    {:result (sort-by :pos
                      (concat (query-internal-docrefs release-id)
                              (query-external-docrefs release-id)))}))

(comment
  (list-docrefs "F-J-SPM-013" "C"))

(defn list-docrefs-to [document-number issue]
  (let [[_ release-id error] (get-release document-number issue)
                                                            :escape (when error  
                                                                      {:error error})]
    {:result
     (map #(assoc (query-release (first %))
                  :pos  (second %))
          (query-docrefs-to release-id))}))

(comment
  (list-docrefs-to "F-J-SCP-027" "1"))

(defn remove-docref [document-number issue pos]
  (let [[_ release-id error] (get-release document-number issue) 
                                                            :escape (when error 
                                                                      {:error error})
        doc-refs (query-docrefs-ents release-id)
        tx (reduce (fn [tx [doc-ref-pos doc-ref-ent]]
                     (cond (= pos doc-ref-pos)
                           (into tx (tx-remove-docref doc-ref-ent))
                           (< pos doc-ref-pos)
                           (into tx (tx-update-docref-pos doc-ref-ent (dec doc-ref-pos)))
                           :else
                           tx))
                   []
                   doc-refs)                                :escape (when (empty? tx)  
                                                                      {:error :non-existing-docref})]
    (d/transact conn tx)
    {:result :success}))

(comment (remove-docref "F-J-SPM-013" "C" 0))

(defn move-docref-up [document-number issue pos]
  (let [:escape (when (zero? pos) {:error :already-first})
        [_ release-id error] (get-release document-number issue)
        :escape (when error {:error error})
        doc-ref (query-docrefs-ents release-id pos)
        :escape (when (empty? doc-ref) {:error :invalid-docref})
        doc-ref (-> doc-ref first first)
        prev-doc-ref (-> (query-docrefs-ents release-id (dec pos)) first first)
        tx (concat (tx-update-docref-pos doc-ref (dec pos))
                   (tx-update-docref-pos prev-doc-ref pos))]
    (d/transact conn tx)
    {:result :success}))

(defn move-docref-down [document-number issue pos]
  (move-docref-up document-number issue (inc pos)))

(comment (move-docref-down "F-J-SPM-013" "C" 0)
         (query-docrefs-ents 2 1))

(defn- gen-tx-for-make-docref-external [document-number issue pos]
  (let [[_ release-id error] (get-release document-number issue)
                                                          :escape (when error 
                                                                    [nil error])
        doc-ref (query-docrefs-ents release-id pos)
                                                          :escape (when (empty? doc-ref) 
                                                                    [nil :invalid-docref])
        doc-ref (-> doc-ref first first)
        about-all-docrefs (query-internal-docrefs release-id)  ; to be optimised !
        about-docref (filter #(= (% :pos) pos) about-all-docrefs)
                                                          :escape (when (empty? about-docref) 
                                                                    [nil :not-internal])
        {number :number issue :issue title :title} (first about-docref)] 
    [(tx-make-docref-external doc-ref (str title ", " number ", issue " issue)) nil ]))

(defn make-docref-external [document-number issue pos] 
  (let [[tx error] (gen-tx-for-make-docref-external document-number issue pos)
                                                          :escape (when error 
                                                                    {:error error})]
    (d/transact conn tx)
    {:result :success}))

(defn- tx-remove-release [issue-ent]
  [[:db/retractEntity issue-ent]])

(defn remove-issue [document-number issue]
  ; todo: Fehler von gen-tx ausfiltern!
  ; todo: externalisierte Documente rückmelden.
  (let [{result :result error :error} (list-docrefs-to document-number issue)
                                                         :escape (when error 
                                                                   {:error error})]
    (->> result
         (map (fn [{number :number issue :issue pos :pos}]
                  (first (gen-tx-for-make-docref-external number issue pos))))
         (apply concat)
         (into (tx-remove-release (second (get-release document-number issue))))
         (d/transact conn))))

(gen-tx-for-make-docref-external "F-J-SPM-013" "C" 0)
(remove-issue "F-J-SCP-027" "1")
(remove-issue "F-J-SDP-027" "B")
(list-releases "F-J-SCP-027")
(list-docrefs "F-J-SPM-013" "C")

(comment
  (remove-issue "F-J-SPM-013" "C" 0))

(comment
  (list-docrefs "F-J-SPM-013" "C")
  (list-docrefs-to "F-J-SCP-027" "1"))

