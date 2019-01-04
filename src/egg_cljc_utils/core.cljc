(ns egg-cljc-utils.core
  #?(:clj (:require [clojure.reflect :refer [reflect]]
                    clojure.set)))

(defn- inspect-1  [expr]
  `(let  [result# ~expr]
     (println  (str  (pr-str '~expr) " => "  (pr-str result#)))
     result#))

(defmacro inspect  [& exprs]
  `(do ~@(map inspect-1 exprs)))

;;;;; The classic break-to-repl of eg python:

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(defn readr [prompt exit-code]
  (let [input (clojure.main/repl-read prompt exit-code)]
    (if (= input :q)
      exit-code
      input)))

(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))

;;
(defmacro break
  ":q to exit"
  []
  `(clojure.main/repl
    :prompt #(print "debug => ")
    :read readr
    :eval (partial contextual-eval (local-context))))

(comment
  (defn f [a]
    (let [x 3
          y 5]
      (break)
      (+ a x y)))

  )

;;;;; End break-to-repl

(defn pretty-spit
  "Useful for ad hoc data pulls. Same args as spit."
  [f content & options]
  (assert f "You're trying to spit to an unnamed file!")
  (clojure.java.io/make-parents f)
  (binding [*print-namespace-maps* false
            *print-length* 10000000]
    (apply spit f
           (str content)
           options)))

(defn print-call-stack
    "Like print-stack-trace, but doesn't wait for an exception. Sometimes
      it's useful to know what called a function, and AFAIK this is the easiest way
      of doing that."
    []
    #?(:clj  (try (throw (Exception. "")) (catch Exception e (.printStackTrace e *out*)))
                 ;; Note that cljs version sends to JS console to get a better stack trace.
                 :cljs (try (throw (js/Error. ""))  (catch js/Error e  (js/console.log e)))))

;; TODO search-key
(defn search-val
  "Given a nested data structure and a desired value which appears in the
  structure, print all paths which will lead to that value (the results, for
  example, could be used directly in a get-in).
  Note that this fn, intended for repl use, prints found paths and returns nil."
  ([m x]
   (search-val m x []))
  ([m x path]
   (cond
     (= x m)         (println "Path to" x ":" path)
     (map? m)        (run!
                      (fn [item]
                        (let [[k v] item]
                          (search-val v x (conj path k))))
                      m)
     (sequential? m) (doall (map-indexed
                             (fn [idx item]
                               (search-val item x (conj path idx)))
                             m))
     (set? m)        (run! (fn [item]
                             (search-val item x (conj path item)))
                           m))))

(defmacro defn!
  "Variant of defn. When the function thus defined is called, each of its
parameters is bound to a top-level var of the same name.  Handles docstrings
and metadata, but pre/post conditions will not be checked by the resulting
function. WARNING: does not attempt to avoid name collisions between parameter
names and existing ns-level vars; may overwrite existing var bindings in the
ns. Suitable for use during development only. Works in both clj and cljs,
although for cljs the usual caveats about macros apply -- requiring the ns at
runtime from the repl seems to work well for defn!.
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
  (let [[front [fn-args & back]] (split-with (complement vector?)
                                             macro-args#)
        inline-defs (for [arg fn-args]
                      `(def ~arg ~arg))]
    `(defn ~@front ~fn-args ~@inline-defs ~@back)))


;;;;;;;; Clojure Reflection & introspection

(do ;; Checking what's been defined in the ns; useful at REPL.
    ;; Clojure-only, unfortunately, because cljs doesn't have enough
    ;; introspection. Maybe with a macro, but not right now.
  (#?@(
       :clj
       [(defn ns-values-map
          "Returns a map from symbol to value for all values defined by the ns. May be
  problematically large if printed!"
          ([] (ns-values-map *ns*))
          ([ns']
           (into {}
                 (for [sym (keys (ns-publics ns'))]
                   [sym (var-get (resolve sym))]))))

        (defn ns-fns
          "Returns a list of symbols for the fns defined in the ns."
          ([] (ns-fns *ns*))
          ([ns']
           (for [[s val'] (ns-values-map *ns*)
                 :when (fn? val')]
             s)))

        (defn ns-variables
          "Returns a list of symbols for the variables defined in the ns, where
  'variables' is sloppy shorthand for non-fn vars."
          ([] (ns-variables *ns*))
          ([ns']
           (clojure.set/difference (set (keys (ns-values-map ns')))
                                   (set (ns-fns ns')))))

        ])))

;;;;;;;; Java Reflection & introspection

(do
  #?@(:clj
      [
       ;;;; Some reflection stuff I'm fooling around with:

       (defn- get-name [^java.lang.reflect.Method m] (.getName m))

       (defn get-methods [obj] (.getMethods ^Class (type obj)))

       (def get-method-names #(map get-name (get-methods %)))

       (def get-method-sigs #(map str (get-methods %)))

       ;;;; End reflection stuff

       ]
      :cljs
      [
       (defn props [obj]
         (js/Object.keys obj))

       ]))
