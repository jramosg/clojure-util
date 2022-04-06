(ns datomic-examples.datomic-examples
  (:require
    [datomic.client.api :as d]
    [clj-time.coerce :as coerce]))

(defn query-with-as-of [db email date-str]
  (d/q
    '[:find ?fn
      :in $ ?email
      :where
      [?u :user/email "jramos@ubikare.io"]
      [?u :user/first-name ?fn]]
    (d/as-of db (java.util.Date. (coerce/to-long date-str)))
    email))

(defn fetch-schema [db]
  (d/q
    '[:find ?attr ?type ?card
      :where
      [_ :db.install/attribute ?a]
      [?a :db/valueType ?t]
      [?a :db/cardinality ?c]
      [?a :db/ident ?attr]
      [?t :db/ident ?type]
      [?c :db/ident ?card]]
    db))

(defn change-cardinality
  "This example changes cardinality of :user/favourite-fruits to many"
  [conn]
  @(d/transact conn [{:db/id :user/favourite-fruits
                      :db/cardinality :db.cardinality/many}]))

(defn comment-count-str [x]
  (str "Comment Count: " (count x)))

(defn- x-form-example [db]
  (d/q '[:find (pull ?posts [{:post/author [:user/first+last-name]}
                             [:post/comments :xform datomic-examples.datomic-examples/comment-count-str]])
         :where [?posts :post/author _]] db))

(defn worker-stats [db worker-eid]
  (let [{:worker/keys [age days-since-disciplinary-incident]} (d/pull db '[:worker/age :worker/days-since-disciplinary-incident] worker-eid)]
    {:worker/stats {:age age :days-since-disciplinary-incident days-since-disciplinary-incident}}))

(defn custom-fn-example [db]
  (d/q '[:find (pull ?farm [:farm/name]) ?worker-stats
         :where [?farm :farm/top-worker ?top-worker]
         [(datomic-examples.datomic-examples/worker-stats $ ?top-worker) ?worker-stats]] db))

(defn nested-pull-in-query [db]
  (let [transfer-id #uuid "59B9C791-74CE-4C51-A4BC-EF6D06BEE2DBA"]
    (d/q '[:find (pull ?transfer [*
                                  {:transfer/from [* {:account/owner [*]}]}
                                  {:transfer/to [* {:account/owner [*]}]}])
           :in $ ?t-id
           :where [?transfer :transfer/id ?t-id]] db transfer-id)))