(ns user)

(def default-refresh-dirs
  "src/com/zougue")

(defmacro jit
  "Just in time loading of dependencies."
  [sym]
  `(requiring-resolve '~sym))

(defn set-prep! []
  ((jit integrant.repl/set-prep!) (fn [] (var-get (jit com.zougue.core/ig-config)))))

(defn go []
  (set-prep!)
  ((jit integrant.repl/go)))

(defn halt []
  (set-prep!)
  ((jit integrant.repl/halt)))

(defn restart []
  (set-prep!)
  (halt)
  (go))

(defn reset []
  (set-prep!)

  ((jit clojure.tools.namespace.repl/clear))
  ((jit clojure.tools.namespace.repl/set-refresh-dirs) default-refresh-dirs)
  ((jit integrant.repl/reset)))

(defn system []
  @(jit integrant.repl.state/system))

(defn config []
  @(jit integrant.repl.state/config))

(defn start-debugger []
  ((jit flow-storm.api/local-connect)))

(defn stop-debugger []
  ((jit flow-storm.api/stop)))
