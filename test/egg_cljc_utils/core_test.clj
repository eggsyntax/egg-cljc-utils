(ns egg-cljc-utils.core-test
  (:require [clojure.test :as t]
            [egg-cljc-utils.core :refer :all]))

(t/deftest dpull-test
  (let [ex {:user/id 12,
            :user/name "Bob",
            :user/address {:address/town "Springfield",
                           :address/zip "11111",
                           :address/georegion {:georegion/name "Central Valley",
                                               :georegion/ave-temp 80}}
            :user/pets [{:pet/name "Tony",
                         :pet/type "Tiger"},
                        {:pet/name "Louis",
                         :pet/type "Lion"}]
            1247 :excuse-for-non-kw-key
            :user/emails #{{:email/name "Gmail", :email/address "foo@gmail.com"}
                           {:email/name "Yahoo", :email/address "foo@yahoo.com"}}}]
    ;; TODO maybe add nested-vector case?
    (t/testing "simple cases"
      (t/is (= {:user/name "Bob"}
               (dpull ex [:user/name])))
      (t/is (= {:user/name "Bob", :user/id 12}
               (dpull ex [:user/name :user/id])))
      (t/is (= {1247 :excuse-for-non-kw-key}
               (dpull ex [1247]))))
    (t/testing "simple nested maps"
      (t/is (= {:user/name "Bob", :user/address {:address/zip "11111"}}
               (dpull ex [:user/name {:user/address [:address/zip]}])))
      (t/is (= {:user/address {:address/zip "11111"}}
               (dpull ex [{:user/address [:address/zip]}])))
      (t/is (= #:user{:address #:address{:georegion #:georegion{:ave-temp 80}}}
               (dpull ex [{:user/address [{:address/georegion [:georegion/ave-temp]}]}]))))
    (t/testing "underspecification of nested structures returns everything (so no need for '*)"
      (t/is (= #:user{:address
                      #:address{:town "Springfield",
                                :zip "11111",
                                :georegion #:georegion{:name "Central Valley", :ave-temp 80}}}
               (dpull ex [:user/address]))))
    (t/testing "sequences in data structure"
      (t/is (= {:user/pets [{:pet/type "Tiger"} {:pet/type "Lion"}]}
               (dpull ex [{:user/pets [:pet/type]}]))))
    (t/testing "sets"
      (t/is (= {:user/emails #{{:email/name "Gmail"}, {:email/name "Yahoo"}}}
               (dpull ex [{:user/emails [:email/name]}]))))
    (t/testing "missing"
      (t/is (= {:user/id 12}
               (dpull ex [:user/id :user/religion])))
      (t/is (= nil
               (dpull ex [{:user/emails [:email/administrator]}])))
      (t/is (= nil
               (dpull ex [{:user/pets [:pet/diseases]}]))))
    (t/testing "nil data structure (pull structure isn't allowed to be nil):"
      (t/is (= nil (dpull nil [:foo/bar]))))))
