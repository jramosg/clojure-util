(ns datomic-examples.datomic-examples
  (:require
    [datomic.api :as d]
    [clj-time.coerce :as coerce]))

(defn query-with-as-of [conn email date-str]
  (d/q
    '[:find ?fn
      :in $ ?email
      :where
      [?u :user/email "jramos@ubikare.io"]
      [?u :user/first-name ?fn]]
    (d/as-of (d/db conn) (java.util.Date. (coerce/to-long date-str)))
    email))

