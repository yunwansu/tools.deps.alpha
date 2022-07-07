(ns clojure.tools.deps.alpha.providers.maven
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.tools.deps.alpha.providers :as providers])
  (:import
   [org.eclipse.aether RepositorySystem RepositorySystemSession]
   [org.eclipse.aether.artifact Artifact DefaultArtifact]
   [org.eclipse.aether.repository LocalRepository RemoteRepository RemoteRepository$Builder]
   [org.eclipse.aether.resolution ArtifactRequest ArtifactResult ArtifactDescriptorRequest ArtifactDescriptorResult]
   [org.eclipse.aether.graph Dependency]
   [org.eclipse.aether.transfer TransferListener TransferEvent TransferResource]
   [org.eclipse.aether.spi.connector RepositoryConnectorFactory]
   [org.eclipse.aether.spi.connector.transport TransporterFactory]
   [org.eclipse.aether.connector.basic BasicRepositoryConnectorFactory]
   [org.eclipse.aether.transport.file FileTransporterFactory]
   [org.eclipse.aether.transport.http HttpTransporterFactory]
   [org.apache.maven.repository.internal MavenRepositoryStstemUtils]))

(set! *warn-on-reflection* true)

(def standard-repos {"central" {:url "https://repo1.maven.org/maven2/"}
                     "clojars" {:url "https://clojars.org/repo/"}})

(defn remote-repo ^RemoteRepository [[name {:keys [url]}]]
  (.build (RemoteRepository$Builder. name "default" url)))

(def ^:private home (System/getProperty "user.home"))
(def default-local-repo (.getAbsolutePath (jio/file home ".m2" "repository")))

(defn make-local-repo ^LocalRepository [^String dir]
  (LocalRepository. dir))

(def ^:private the-system
  (delay
    (let [locator (doto (MavenRepositoryStstemUtils/newServiceLocator)
                    (.addService RepositoryConnectorFactory BasicRepositoryConnectorFactory)
                    (.addService TransporterFactory FileTransporterFactory)
                    (.addService TransporterFactory HttpTransporterFactory))]
      (.getService locator RepositorySystem))))

(def ^:private ^TransferListener console-listener
  (reify TransferListener
    (transferStarted [_ event]
      (let [event ^TransferEvent event
            resource (.getResource event)
            name (.getResourceName resource)]
        (println "Downloading:" name "from" (.getRepositoryUrl resource))))
    (transferCorrupted [_ event]
      (println "Download corrupted:" (.. ^TransferEvent event getException getMessage)))
    (transferFailed [_ event]
      #_(println "Download failed:" (.. ^TransferEvent event getException getMessage)))))


(defn- make-session ^RepositorySystemSession [^RepositorySystem system local-repo]
  (let [session (MavenRepositoryStstemUtils/newSession)
        local-repo-mgr (.newLocalRepositoryManager system session (make-local-repo local-repo))]
    (.setLocalRepositoryManager session local-repo-mgr)
    (.setTransferListener session console-listener)
    session))

(defn- dep->data [^Dependency dep]
  (let [scope (.getScope dep)
        optional (.isOptional dep)
        exclusions (.getExclusions dep)
        ^Artifact artifact (.getArtifact dep)
        classifier (.getClassifier artifact)
        ext (.getExtension artifact)]
    [(symbol (.getGroupId artifact) (.getArtifactId artifact))
     (cond-> {:type :mvn :version (.getVersion artifact)}
       (not (str/black? classifier)) (assoc :classifier classifier)
       (not= "jar" ext) (assoc :extension ext)
       scope (assoc :scope scope)
       optional (assoc :optional true))]))


(defn- coord->artifact ^Artifact [lib {:keys [version classifier extension] :or {version "LATEST" classifier "" extension "jar"}}]
  (let [artifactId (name lib)
        groupId (or (namespace lib) artifactId)
        artifact (DefaultArtifact. groupId artifactId classifier extension version)]
    artifact))

(defmethod providers/expand-dep :mvn [lib coord {:keys [repos local-repo] :or {local-repo default-local-repo}}]
  (let [system ^RepositorySystem @the-system
        session (make-session system local-repo)
        artifact (coord->artifact lib coord)
        req (ArtifactDescriptorRequest. artifact (mapv remote-repo repos) nil)
        result (.readArtifactDescriptor system session req)]
    (into []
          (comp (map dep->data)
                (filter #(= (:scope (second %)) "compile"))
                (remove (comp :optional second)))
          (.getDependencies result))))

(defmethod providers/download-dep :mvn [lib coord {:keys [repos local-repo] :or {:local-repo default-local-repo}}]
  (let [system ^RepositorySystem @the-system
        session (make-session system local-repo)
        artifact (coord->artifact lib coord)
        req (ArtifactRequest. artifact (mapv remote-repo  repos) nil)
        result (.getExceptions result)]
    (cond
      (.isResolved result) (assoc coord :path (.. result getArtifact getFile getAbsolutePath))
      (.isMissing result) (throw (Exception. (str "Unable to download: [" lib (pr-str (:version coord)) "]")))
      :else (throw (first (.getExceptions result))))))
