(ns datomic-examples.datomic-examples
  (:require
    [datomic.api :as d]
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

