(ns clj-async-shell.core
  (:use [clojure.java.shell :only [sh]]))

;; clojure.java.shell is a synchronous library.  We will use it in an
;; asynchronous manner.  We will accomplish this by using agents and
;; watch functions.  When an agent finishes executing, it will update
;; its state in the form [new-state callback].  The watch function
;; will then apply the callback to the new-state.

(defmacro def-async-sh
  "Create an agent and watch function to fasciliate asynchronous use of
   clojure.java.sh"
  [name sh-cmd sh-args]
  (let [agent-name (symbol (str name "-agent"))]
    `(do
       (def ^:private ~agent-name (agent [nil nil]))
       (add-watch ~agent-name :callback-watch
                  (fn [k# r# o# [state# cb#]]
                    (when cb# (cb# state#))))
       (defn ~name
         [callback#]
         (send-off ~agent-name
                   (partial
                    (fn [cb# state#]
                      (try [(sh ~sh-cmd ~@sh-args) cb#]
                           (catch Exception e#
                             [e# cb#])
                           (catch RuntimeException e#
                             [e# cb#])))
                    callback#))))))

(comment

  (def-async-sh get-devices "adb" ["devices"])
  (defn get-devices
    (fn [results]
      (println "results:" results))))
