(ns clj-async-shell.core
  (:use [clojure.java.shell :only [sh]]))

;; clojure.java.shell is a synchronous library.  We will use it in an
;; asynchronous manner.  We will accomplish this by using agents and
;; watch functions.  When an agent finishes executing, it will update
;; its state in the form [new-state callback].  The watch function
;; will then apply the callback to the new-state.

#_(defn fn-args
  [f]
  (when (symbol? f)
    (:arglists (meta (resolve f)))))

(defn- applicator
  [fn-spec]
  `(~fn-spec ~@(second fn-spec)))

(defn- make-function-transform
  [arg params sh-args-vector]
  (let [[func & rest] arg]
    (cond (= func 'fn) (do (conj! params (second arg))
                           (conj! sh-args-vector (applicator arg)))
          :else (do (conj! params (vec (filter symbol? rest)))
                    (conj! sh-args-vector arg)))))

(defn- eval-sh-args-spec
  [args]
  (loop [params (transient [])
         sh-args-vector (transient [])
         [arg & args] (seq args)]
    (if arg
      (do (cond
           ;; if arg is a constant, then pass it directly to the shell
           ;; command
           (or (string? arg)
               (number? arg)) (conj! sh-args-vector (str arg))

           ;; if arg is a symbol, then we need to get the symbol's
           ;; value at runtime
           (symbol? arg) (do (conj! params arg)
                             (conj! sh-args-vector arg))

           ;; if arg is a list, then treat it as a function.  Put the
           ;; specified function's parameters in the arglist and then
           ;; the specified function to their values.  The result of this is
           ;; then passed to the shell command
           (list? arg) (make-function-transform arg params sh-args-vector))

          (recur params sh-args-vector args))
      [(persistent! params) (persistent! sh-args-vector)])))

(defmacro def-async-sh
  "Create an agent and watch function to facilitate asynchronous use of
   clojure.java.sh.

   sh-args-spec is a sequence of:
     string | number | symbol | fn-spec

   strings and numbers: passed directly into the command line
   symbols: passed to the function at call-time.  The value of the
            symbol is then passed to the command line
   fn-spec: a list of the form:
     (fn [<params>] body) | (arbitrary-fn-name <params>)

     The specified function must return a string. The specified
     parameters become required parameters for the generated function.
     The parameter values are passed into the function, and the
     resulting string is passed to the command line."
  [name sh-cmd & sh-args-spec]
  (let [agent-name (symbol (str name "-agent"))
        [params sh-args-vector] (eval-sh-args-spec sh-args-spec)
        callback (gensym)]
    `(do
       (def ^:private ~agent-name (agent [nil nil]))
       (add-watch ~agent-name :callback-watch
                  (fn [k# r# o# [state# cb#]]
                    (when cb# (cb# state#))))
       (defn ~name
         ~(conj params callback)
         (send-off ~agent-name
                   (partial
                    (fn [cb# state#]
                      (try [(sh ~sh-cmd ~@sh-args-vector) cb#]
                           (catch Exception e#
                             [e# cb#])
                           (catch RuntimeException e#
                             [e# cb#])))
                    ~callback))))))

(comment

  (def-async-sh get-devices "adb" "devices")
  (get-devices
   (fn [results]
     (println "results:" results)))

  (defn format-port-spec
    [& {:keys [type port]}]
    (format "%s:%d" (.toLowerCase type) port))
  (def-async-sh forward-port "adb" "-s" device "forward"
    (format-port-spec :type src-type :port src-port)
    (format-port-spec :type dst-type :port dst-port))
  (forward-port "DEVICE-ID" ["tcp" 8888] [:tcp 9999]
                (fn [results] (println "results:" results)))
  
  )
