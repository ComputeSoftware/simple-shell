(ns compute.simple-shell.core
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
    [compute.threadolet :as threadolet])
  (:import (java.lang ProcessBuilder$Redirect)))

(defn exit?
  "Returns true if `x` is an exit map."
  [x]
  (some? (::status x)))

(defn success?
  [r]
  (= 0 (::status r)))

(defn failed?
  [r]
  (> (::status r 0) 0))

(defn exit-map
  "Return a map that can be passed to `system-exit` to exit the process."
  ([ok?-or-status] (exit-map ok?-or-status nil))
  ([ok?-or-status msg]
   (let [status (if (number? ok?-or-status)
                  ok?-or-status
                  (if ok?-or-status 0 1))
         ok? (= status 0)]
     (cond-> {::status status
              ::ok?    ok?}
             msg (assoc ::message msg)))))

(defn system-exit!
  "Exit the process."
  [{::keys [status message]}]
  (when (and message (not (str/blank? message)))
    (binding [*out* (if (= 0 status) *out* *err*)]
      (println message)))
  (System/exit status))

(defn exit!
  ([ok?-or-status] (exit! ok?-or-status nil))
  ([ok?-or-status msg]
   (system-exit! (exit-map ok?-or-status msg))))

(defmacro alet
  [bindings & body]
  (threadolet/let-template (partial threadolet/short-circuit failed?) bindings body))


;; we need to write our own shell function instead of Clojure's because of
;; https://dev.clojure.org/jira/browse/CLJ-959
(defn spawn-sync-jvm
  ([args] (spawn-sync-jvm args nil))
  ([args {:keys [redirect dir]}]
   (let [redirect (cond
                    (or (map? redirect) (nil? redirect)) (merge {:input :pipe :output :pipe :error :pipe} redirect)
                    (keyword? redirect) {:input redirect :output redirect :error redirect})
         builder (ProcessBuilder. ^"[Ljava.lang.String;" (into-array String args))
         do-redirect* (fn [redirect-fn redirect-type]
                        (case redirect-type
                          :pipe (redirect-fn builder ProcessBuilder$Redirect/PIPE)
                          :inherit (redirect-fn builder ProcessBuilder$Redirect/INHERIT)))]
     ;; redirect stdio
     (do-redirect* (memfn redirectInput redirect-type) (:input redirect))
     (do-redirect* (memfn redirectOutput redirect-type) (:output redirect))
     (do-redirect* (memfn redirectError redirect-type) (:error redirect))
     ;; set cwd
     (when dir
       (.directory builder (io/file dir)))
     ;; start the process
     (let [proc (.start builder)]
       (with-open [stdout (.getInputStream proc)
                   stderr (.getErrorStream proc)]
         (let [status (.waitFor proc)]
           {::status status
            ::out    (str/trim-newline (slurp stdout))
            ::err    (slurp stderr)}))))))

(defn sh
  ([cli-args] (sh cli-args nil))
  ([cli-args opts]
   (let [args (filter some? cli-args)]
     (spawn-sync-jvm (map str args) opts))))