(ns clojure.tools.deps.alpha.providers)

(defmulti expand-dep (fn [lib coord provider] (:type coord)))

(defmulti download-dep (fn [lib coord provider] (:type coord)))

