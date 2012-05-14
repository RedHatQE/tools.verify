(ns tools.verify
  (:use [clojure.string :only [split]]))

(defn ^{:private true} local-bindings
  "Produces a map of the names of local bindings to their values."
  [env]
  (let [symbols (keys env)]
    (zipmap (for [sym symbols] `(quote ~sym)) symbols)))

(defn symbols [sexp]
  "Returns just the symbols from the expression, including those
   inside literals (sets, maps, lists, vectors)."
  (distinct (filter symbol? (tree-seq coll? seq sexp))))

(defn used-bindings [m form]
  (select-keys m (symbols form)))

(defn check [tval form bindings err]
  (if (not tval)
    (let [sep (System/getProperty "line.separator")
          msg (apply str "Verification failed: "
                     (pr-str form) sep
                     (for [[k v] bindings] (str "\t" k " : " (pr-str v) sep)))
          e (AssertionError. msg)]
      (when err (.initCause e err))
      (throw e))
    tval))

(defmacro verify-that [x]
  (let [bindings (local-bindings &env)]
    `(let [err# (atom nil)
           res# (try ~x (catch Exception e# (do (reset! err# e#) nil)))
           form# '~x]
       (check res# form# (used-bindings ~bindings form#) @err#))))
