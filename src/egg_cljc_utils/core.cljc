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

