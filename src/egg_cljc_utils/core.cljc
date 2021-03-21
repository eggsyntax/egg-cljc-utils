(ns egg-cljc-utils.core
  #?(:clj (:require [clojure.reflect :refer [reflect]]
                    clojure.set
                    [clojure.test :as t])))

(defn- inspect-1  [expr]
  `(let  [result# ~expr]
     (println  (str  (pr-str '~expr) " => "  (pr-str result#)))
     result#))

(defmacro inspect  [& exprs]
  `(do ~@(map inspect-1 exprs)))

;;;;; Core fns that I wish existed

(defn separate-with
  "Returns a two-item vector of the elements of coll for which pred returns
  true, and the elements for which it returns false. Similar to
  clojure.core/split-with, but doesn't require a sorted coll."
  [pred coll]
  (reduce (fn [[ys ns] item]
            (if (pred item)
              [(conj ys item)        ns      ]
              [      ys        (conj ns item)]))
          [(empty coll) (empty coll)]
          coll))

;;;;; The classic break-to-repl of eg python:

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(defn readr [prompt exit-code]
  (let [input (clojure.main/repl-read prompt exit-code)]
    (println input)
    (if (or (= input :q) (= input 'q))
      exit-code
      input)))

(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))

(defmacro break
  ":q to exit"
  []
  `(clojure.main/repl
    :prompt #(print "[:q quits] debug => ")
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

(defmacro defp
  "Like def, but prints its output before binding the var"
  [nme form]
  `(let [result# ~form]
     (println (str "Result of " '~form ":"))
     (println result#)
     (def ~nme result#)))

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


;; Data searching and munging

;; TODO found a case where this breaks on (search-key ctx :state) -- see bottom for ctx
(defn search-key
  "Given a nested data structure and a desired key which appears in the
  structure, return all paths which will lead to that key (including as the last
  element the key itself). These paths can often be used directly in a get-in or
  a threading macro."
  ([m ky]
   (into [] (remove nil? (search-key m ky []))))
  ([m ky path]
   (when (coll? m)
     (if (contains? m ky)
       [(conj path ky)] ; success!
       (cond
         ;; (= m ky) path
         (map? m)        (mapcat
                          (fn [[k v]]
                            (search-key v ky (conj path k)))
                          m)
         (sequential? m) (apply concat
                                (map-indexed
                                 (fn [idx item]
                                   (search-key item ky (conj path idx)))
                                 m))
         (set? m)        (mapcat
                          (fn [item]
                            ;; Note: (conj path item) can make for an awkward
                            ;; path since item may be a large, complex structure.
                            (search-key item ky (conj path item)))
                          m))))))

(defn search-val
  "Given a nested data structure and a desired value which appears in the
  structure, print all paths which will lead to that value (the results, for
  example, can often be used directly in a get-in or a threading macro).
  Note that this fn, intended for repl use, prints found paths and returns nil."
  ([m x]
   (search-val m x []))
  ([m x path]
   (cond
     (= x m)         (println "Path to" x ":" path)
     (map? m)        (run!
                      (fn [[k v]]
                        (search-val v x (conj path k)))
                      m)
     (sequential? m) (doall (map-indexed
                             (fn [idx item]
                               (search-val item x (conj path idx)))
                             m))
     (set? m)        (run! (fn [item]
                             (search-val item x (conj path item)))
                           m))))

(defn- seq-into
  "Like into, but returns nil if existing-seq is empty or contains only nils."
  [empty-coll existing-seq]
  (let [nil-free-existing-seq (remove nil? existing-seq)]
    (when (seq nil-free-existing-seq)
     (into empty-coll nil-free-existing-seq))))

(defn dpull*
  "Inner implementation of dpull"
  [d p]
  (when (and d p)
    (cond
      (sequential? p) ; seq containing keys for a collection
      (do
        (assert (or (map? d) (set? d) (sequential? d)) (str "Pull vector " p " can't be applied to a " (type d) ", only to a map or sequence."))
        (apply merge
               (seq-into []
                (map (partial dpull* d) p))))

      (map? p) ; specification for a key and its substructure
      (do
        (assert (= 1 (count p)) (str "Pull syntax map " p " can only contain one item."))
        (let [[k v] (first p)]
          (assert (vector? v) (str "Value in pull syntax map " p " must be a vector; " v " is a " (type v) "."))
          ;; k tells us what substructure to get from d; v describes what should
          ;; be pulled from that substructure.
          (when (contains? d k)
            (when-let [v-pull (dpull* (get d k) v)]
              {k v-pull}))))

      :else ; treat this element as a key which must be present in the current data structure
      (do
        (assert (or (map? d) (set? d) (sequential? d)) (str "Pull vector " p " can't be applied to a " (type d) ", only to a map or sequence."))
        (if (or (sequential? d) (set? d)) ; seq-ish of entities; apply pull to each
          (seq-into (empty d)
                    (remove nil?
                            (mapv #(dpull* % p) d)))
          (when-let [v (get d p)] {p v}) ; simple map retrieval
          )))))

;; Alternatives:
;; - Juxt has a full-featured library for doing the same thing: https://github.com/juxt/pull
;; - Meander and Specter are both libraries for performing complex searches & transformations on Clojure data
(defn dpull
  "Pull data from a nested data structure d, using Datomic pull structure p.
  Matches Datomic pull behavior in most ways, but it's unnecessary to use '* for
  'all attributes'; simply including :foo/bar without further specification is
  sufficient to return all substructure of :foo/bar."
  [d p]
  (assert (vector? p) "Top-level structure of pull specification must be a vector.")
  (dpull* d p))

;; Variant repls & related

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
    ;; Clojure-only, unfortunately, because cljs doesn't have var-get.
  #?@(
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

        ]))

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



;; TODO DELETE-ME after using to debug search-key
(comment ; ctx that breaks search-key

{:reg-at-polling-place false,
 :voter-registration-status-url "https://www.mvp.sos.ga.gov/MVP/mvp.do",
 :reg-deadline-passed false,
 :local-reg-authority-lookup-url
 "https://elections.sos.ga.gov/Elections/countyregistrars.do",
 :recipient
 {:party :democratic,
  :paper-vbm-form false,
  :addresses
  {:registered
   {:city "Athens",
    :state "GA",
    :street "2450 S Milledge Ave",
    :zip "30605",
    :ocd-divisions
    #{"ocd-division/country:us/state:ga"
      "ocd-division/country:us/state:ga/place:athens" "ocd-division/country:us"
      "ocd-division/country:us/state:ga/county:clarke"
      "ocd-division/country:us/state:ga/cd:10"}}},
  :email "t@t.no",
  :last-name "McTesterson",
  :should-request-vbm-form false,
  :create-session #uuid "99f6f191-d0cb-4e56-87ee-310ccccf2eca",
  :state "Georgia",
  :hostname "localhost",
  :first-name "GA",
  :vbm-form-available true,
  :language-preference :en,
  :id #uuid "5ea1ec5d-f55b-4032-95b2-97ca8cdcd746",
  :full-name "GA McTesterson",
  :voting-preference {:in-person true},
  :election-mail-subscription false},
 :local-election-authority-lookup-url
 "https://elections.sos.ga.gov/Elections/countyelectionoffices.do",
 :election
 {:id #uuid "5ea1b0a8-3c51-4892-b8c2-db54cd4121ba",
  :description "Georgia All Active Reg Voters VBM Primary2",
  :date "Wednesday, May 12",
  :primary-election-instructions
  {:email
   "Georgia holds open presidential primary elections, which means all voters, regardless of party, can participate. On Election Day, you may choose either party’s primary ballot. This decision does not register you with that party.",
   :sms
   "Find out more about your state's eligibility requirements for the 2020 presidential primary at: https://tvote.org/2uKsnez"}},
 :voter-registration-status-url-shortened "https://tvote.org/2m2WW5S",
 :signers "Ciru, Solomon, Quinn,",
 :election-authority
 {:official-title "County Election Supervisor",
  :office-name "Clarke County",
  :address-segments ("Clarke County" "PO BOX 1828" "ATHENS GA 30603"),
  :ballot-request-url-download "http://tvote.org/2mG8BeF",
  :vbm-encouraged-type :mailing-requests-to-all-active,
  :email "charlotte.sosebee@accgov.com",
  :phone "(706) 613-3150",
  :lookup nil},
 :reg-at-office false,
 :voting-method {:no-excuse-vbm true},
 :reg-same-day nil,
 :deadlines
 {:registration
  {:deadline-postmarked "Wednesday, May 5",
   :deadline-online "Wednesday, May 5"},
  :vbm {:date "Wednesday, May 5"}},
 :early-voting
 {:primary false,
  :start "Tuesday, May 18",
  :type :early-voting,
  :excuse-required false,
  :url "https://elections.sos.ga.gov/Elections/advancedVotingInfo.do",
  :end "Sunday, May 30",
  :exact-dates false}}

)
