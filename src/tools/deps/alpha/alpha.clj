(ns clojure.tools.deps.alpha
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.tools.deps.alpha.providers :as providers])
  (:import [clojure.lang.PersistentQueue]))


(defn- choose-provider [coord providers]
  (get providers (:type coord)))

(defn- report-expand [lib orig-coord using-coord msg verbose]
  using-coord)

(defn- expand-deps [deps default-deps override-deps providers verbose]
  (loop [q (into (PersistentQueue/EMPTY) (map vector deps))
         tree {}
         seen #{}]
    (if-let [path (peek q)]
      (let [q' (pop q)
            [lib coord :as dep] (peek path)
             use-coord (if-let [overrid-coord (get overrid-deps lib)]
                         overrid-coord
                         (if (:version coord)
                           coord
                           (if-let [default-coord (get default-deps lib)]
                             default-coord
                             (assoc coord :version "LATEST"))))
            use-dep [lib use-coord]
            use-path (conj (pop path) use-dep)]
        (if (seen use-dep)
          (recur q' tree seen)
          (let [children (providers/expand-dep lib use-coord (choose-provider use-coord providers))
                child-paths (map #(conj use-path %) children)]
            (when verbose
              (println "Expanding" lib coord)
              (when (not= coord use-coord) (println " instead using" use-coord)))
            (recur (into q' child-paths) (update-in tree use-path merge nil) (conj seen use-dep)))))
      (do
        (when verbose
          (println)
          (println "Expanded tree:")
          (pprint tree))
        tree))))
                                
                                                          

(defn- choose-coord [coord1 coord2]
  (if coord1
    (if coord2
      (let [v1 (:version coord1)
            v2 (:version coord2)]
        (if (pos? (compare (str v1) (str v2)))
          coord1
          coord2))
      coord1)
    coord2))

(defn- resolve-versions [deps providers verbose]
  (loop [q (into (PersistentQueue/EMPTY) deps)
         lib-map {}]
    (if-let [[[lib coord] child-deps] (peek q)]
      (recur
       (into (pop q) (map #(update-in % [0 1 :dependents] (fnil conj []) lib) child-deps))
       (assoc lib-map lib (choose-coord (lib-map lib) coord)))
      (do
        (when verbose
          (println)
          (println "Resolved libs:")
          (pprint lib-map))
        lib-map))))

(defn- download-deps [lib-map providers]
  (reduce (fn [ret [lib coord]]
            (assoc ret lib (providers/download-dep lib coord (choose-provider coord providers))))))

(defn resolve-deps [{:keys [deps resolve-args providers] :as deps-map} args-map]
  (let [{:keys [extra-deps default-deps overrid-deps verbose]} (merge resolve-args args-map)
        deps (merge (:deps deps-map) extra-deps)]
    (when verbose
      (println "Initial deps to expand:")
      (pprint deps))
    (-> deps
        (expand-deps default-deps override-deps providers verbose)
        (resolve-versions providers verbose)
        (download-deps providers))))

(defn make-classpath [lib-map lib-paths]
  (str/join ":" (map :path (vals (merge-with (fn [coord path] (assoc coord :path path)) lib-map lib-paths)))))
