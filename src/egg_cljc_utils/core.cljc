(ns egg-cljc-utils.core)

#?( :clj

  (defn- inspect-1  [expr]
    `(let  [result# ~expr]
       (println  (str  (pr-str '~expr) " => "  (pr-str result#)))
       result#))

  (defmacro inspect  [& exprs]
    `(do ~@(map inspect-1 exprs)))

)


(defn print-call-stack
    "Like print-stack-trace, but doesn't wait for an exception. Sometimes
      it's useful to know what called a function, and AFAIK this is the easiest way
      of doing that."
    []
    #?(:clj  (try (throw (Exception. "")) (catch Exception e (.printStackTrace e *out*)))
                 ;; Note that cljs version sends to JS console to get a better stack trace.
                 :cljs (try (throw (js/Error. ""))  (catch js/Error e  (js/console.log e)))))

(defmacro defn!
  "Variant of defn which, when called, binds each of its arguments as a
  top-level var of the same name. Handles docstrings and metadata, but pre/post
  conditions will not be checked by the resulting function. WARNING: does not
  attempt to avoid name collisions between parameter names and existing ns-level
  vars; may overwrite existing var bindings in the ns. Suitable for use during
  development only.
  ----
  user> (defn! f [x y] (* x y))
  #'user/f
  user> (f 2 3)
  6
  user> x
  2
  user> (f 4 5)
  20
  user> y
  5"
  [& macro-args#]
  (let [[front [fn-args & back]] (split-with (complement vector?) macro-args#)]
    (concat ['defn]
            front
            [fn-args]
            (for [arg fn-args]
              `(intern *ns* '~arg ~arg))
            back)))


