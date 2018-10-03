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
  (= 0 (::exit r)))

(defn failed?
  [r]
  (> (::exit r 0) 0))

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
  ([args {:keys [stdio dir]}]
   (let [[stdin-opt stdout-opt stderr-opt] (let [[stdin stdout stderr] stdio]
                                             [(or stdin "pipe")
                                              (or stdout "pipe")
                                              (or stderr "pipe")])
         builder (ProcessBuilder. ^"[Ljava.lang.String;" (into-array String args))]
     ;; configure builder
     (case stdin-opt
       "pipe" (.redirectInput builder ProcessBuilder$Redirect/PIPE)
       "inherit" (.redirectInput builder ProcessBuilder$Redirect/INHERIT))
     (case stdout-opt
       "pipe" (.redirectOutput builder ProcessBuilder$Redirect/PIPE)
       "inherit" (.redirectOutput builder ProcessBuilder$Redirect/INHERIT))
     (case stderr-opt
       "pipe" (.redirectError builder ProcessBuilder$Redirect/PIPE)
       "inherit" (.redirectError builder ProcessBuilder$Redirect/INHERIT))
     (when dir
       (.directory builder (io/file dir)))
     (let [proc (.start builder)]
       (with-open [stdout (.getInputStream proc)
                   stderr (.getErrorStream proc)]
         (let [status (.waitFor proc)]
           {::exit status
            ::out  (str/trim-newline (slurp stdout))
            ::err  (slurp stderr)}))))))

(defn sh
  ([cli-args] (sh cli-args nil))
  ([cli-args opts]
   (let [args (filter some? cli-args)]
     (spawn-sync-jvm (map str args) opts))))