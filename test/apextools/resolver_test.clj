(ns apextools.resolver-test
  (:require [clojure.test :refer :all]
            [apextools.package :as pkg]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [apextools.resolver :as r]))



;; we skip the ast representation by overwriting the following functions to
;; simulate results for super-class-type-node, derived-class? and super-class-qname

(def fixture {:derived {}})

(defn- super-class-type-node-mock
  [pkgs qname]
  (get-in fixture [:derived qname]))

(defn- qualified-name-mock
  [node]
  node)

(defn- classes-mock
  [pkgs ns]
  (filter (fn [[n1 n2 n3]] (= ns n1)) (keys (:derived fixture))))

(defn- inner-classes-mock
  [pkgs q]
  (let [[namespace name inner] q]
    (filter (fn [[n1 n2 n3]]
              (and (= namespace n1)
                   (= name n2)
                   n3
                   (nil? inner)))       ;; there is no inner class in an inner class
            (keys (:derived fixture)))))

(defn unresolved-classes-mock
  [pkgs g]
  (->>(keys (:derived fixture))
      (remove #(uber/has-node? g %))))

(defn exists-mock
  [pkgs qname]
  (some (fn [[n1 n2 n3 :as q]]
          (or (and (= 1 (count qname)) (= n1 (qname 0)))
              (= qname q)))
        (keys (:derived fixture))))


(defn- update-inheritance-add-custom-sobject-classes-mock [pkgs g] g)

(defn- update-inheritance-add-default-sobject-classes-mock [g] g)

(def redefs {#'r/super-class-type-node super-class-type-node-mock
             #'r/qualified-name qualified-name-mock
             #'r/update-inheritance-add-custom-sobject-classes update-inheritance-add-custom-sobject-classes-mock
             #'r/update-inheritance-add-default-sobject-classes update-inheritance-add-default-sobject-classes-mock
             #'r/unresolved-classes unresolved-classes-mock
             #'pkg/exists? exists-mock
             #'pkg/classes classes-mock
             #'pkg/inner-classes inner-classes-mock})

(defn- calculate-inheritance-graph-only-classes []
  (with-redefs-fn redefs
    #(r/calculate-inheritance-graph {"foobar" {:classes {}}})))


;; note: derived map :=  fully qualified resolved name --> unresolved context dependent name

(defn- rslv [g n m]
  (with-redefs-fn redefs
    #(r/resolve-class-name {"foobar" {:classes {}}} g n m)))

(deftest test-inheritance-graph-calculation
  (testing "No classes."
    (with-redefs [fixture {:derived {}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-node? g ["void"] ))
        (is (uber/has-node? g ["object"]))
        (is (= 2 (count (uber/nodes g))) "number of nodes")
        (is (= 0 (count (uber/edges g))) "number of edges"))))

  
  (testing "Classes without inheritance."
    (with-redefs [fixture {:derived {["test0" "a"] nil
                                     ["test0" "b"] nil
                                     ["test0" "a" "c"] nil
                                     ["test0" "b" "c"] nil}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-node? g ["test0" "a"]))
        (is (uber/has-node? g ["test0" "b"]))
        (is (uber/has-node? g ["test0" "a" "c"]))
        (is (uber/has-node? g ["test0" "b" "c"]))
        (is (= 6 (count (uber/nodes g))) "number of nodes")
        (is (= 0 (count (uber/edges g))) "number of edges")
        (is (= ["test0" "a"] (rslv g ["test0" "a"] ["a"])))
        (is (= ["test0" "a" "c"] (rslv g ["test0" "a" "c"] ["c"])))
        (is (= ["test0" "a" "c"] (rslv g ["test0" "a" "c"] ["a" "c"])))
        (is (= ["test0" "a" "c"] (rslv g ["test0" "a" "c"] ["test0" "a" "c"]))))))

  
  (testing "Classes with inheritance 1, simple cases."
    (with-redefs [fixture {:derived {["test1" "a"] ["b"]
                                     ["test1" "b"] ["c"]
                                     ["test1" "c"] nil
                                     ["test1" "x"] nil
                                     ["test1" "u"] nil
                                     ["test1" "w"] nil
                                     ["test1" "x" "y"] ["u" "v"]
                                     ["test1" "u" "v"] ["w" "p"]
                                     ["test1" "w" "p"] nil}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-edge? g ["test1" "a"] ["test1" "b"]))
        (is (uber/has-edge? g ["test1" "b"] ["test1" "c"]))
        (is (uber/has-edge? g ["test1" "x" "y"] ["test1" "u" "v"]))
        (is (uber/has-edge? g ["test1" "u" "v"] ["test1" "w" "p"]))
        (is (= 11 (count (uber/nodes g))) "number of nodes")
        (is (= 4 (count (uber/edges g))) "number of edges")
        (is (= ["test1" "a"] (rslv g ["test1" "a"] ["a"])))
        (is (= ["test1" "b"] (rslv g ["test1" "a"] ["b"])))
        (is (= ["test1" "c"] (rslv g ["test1" "a"] ["c"])))
        (is (= ["test1" "w" "p"] (rslv g ["test1" "x" "y"] ["p"])))
        (is (= ["test1" "w" "p"] (rslv g ["test1" "x" "y"] ["w" "p"])))
        (is (= ["test1" "w" "p"] (rslv g ["test1" "x" "y"] ["test1" "w" "p"]))))))

  
  (testing "Classes with inheritance 2, simple cases with namespace."
    (with-redefs [fixture {:derived {["test2" "a"] ["test2" "b"]
                                     ["test2" "b"] ["test2" "c"]
                                     ["test2" "c"] nil
                                     ["test2" "x"] nil
                                     ["test2" "u"] nil
                                     ["test2" "w"] nil
                                     ["test2" "x" "y"] ["test2" "u" "v"]
                                     ["test2" "u" "v"] ["test2" "w" "p"]
                                     ["test2" "w" "p"] nil}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-edge? g ["test2" "a"] ["test2" "b"]))
        (is (uber/has-edge? g ["test2" "b"] ["test2" "c"]))
        (is (uber/has-edge? g ["test2" "x" "y"] ["test2" "u" "v"]))
        (is (uber/has-edge? g ["test2" "u" "v"] ["test2" "w" "p"]))
        (is (= 11 (count (uber/nodes g))) "number of nodes")
        (is (= 4 (count (uber/edges g))) "number of edges"))))


  (testing "Classes with inheritance 3, more complicated cases with inner classes."
    (with-redefs [fixture {:derived {["test3" "a"] nil
                                     ["test3" "a" "o1"] nil
                                     ["test3" "a" "o2"] nil
                                     ["test3" "b"] nil
                                     ["test3" "c"] nil
                                     ["test3" "c" "d"] ["a"]
                                     ["test3" "e"] ["c" "d"]
                                     ["test3" "e" "o2"] nil
                                     ["test3" "f"] nil
                                     ["test3" "f" "g"] ["e"]
                                     ["test3" "h"] ["f" "g"]
                                     ["test3" "h" "i"] ["o1"]
                                     ["test3" "h" "j"] ["o2"]}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-edge? g ["test3" "c" "d"] ["test3" "a"]))
        (is (uber/has-edge? g ["test3" "e"] ["test3" "c" "d"]))
        (is (uber/has-edge? g ["test3" "f" "g"] ["test3" "e"]))
        (is (uber/has-edge? g ["test3" "h"] ["test3" "f" "g"]))
        (is (uber/has-edge? g ["test3" "h" "i"] ["test3" "a" "o1"]))
        (is (uber/has-edge? g ["test3" "h" "j"] ["test3" "e" "o2"]))
        (is (= 15 (count (uber/nodes g))) "number of nodes")
        (is (= 6 (count (uber/edges g))) "number of edges")
        (is (= ["test3" "a" "o1"] (rslv g ["test3" "h"] ["o1"])))
        (is (= ["test3" "e" "o2"] (rslv g ["test3" "h"] ["o2"])))
        (is (= ["test3" "a" "o1"] (rslv g ["test3" "h" "i"] ["o1"])))
        (is (= ["test3" "a" "o2"] (rslv g ["test3" "h" "i"] ["o2"])))
        (is (= ["test3" "a" "o1"] (rslv g ["test3" "h" "j"] ["o1"])))
        (is (= ["test3" "e" "o2"] (rslv g ["test3" "h" "j"] ["o2"])))
        (is (= ["test3" "a" "o1"] (rslv g ["test3" "f" "g"] ["o1"])))
        (is (= ["test3" "e" "o2"] (rslv g ["test3" "f" "g"] ["o2"])))
        (is (= ["test3" "a" "o1"] (rslv g ["test3" "e"] ["o1"])))
        (is (= ["test3" "e" "o2"] (rslv g ["test3" "e"] ["o2"])))
        (is (= ["test3" "a" "o1"] (rslv g ["test3" "e" "o2"] ["o1"])))
        (is (= ["test3" "e" "o2"] (rslv g ["test3" "e" "o2"] ["o2"])))
        (is (nil? (rslv g ["test3" "c"] ["o1"])))
        (is (nil? (rslv g ["test3" "c"] ["o2"])))
        (is (= ["test3" "a" "o1"] (rslv g ["test3" "c" "d"] ["o1"])))
        (is (= ["test3" "a" "o2"] (rslv g ["test3" "c" "d"] ["o2"])))
        (is (nil? (rslv g ["test3" "b"] ["o1"])))
        (is (nil? (rslv g ["test3" "b"] ["o2"])))
        (is (= ["test3" "a" "o1"] (rslv g ["test3" "a"] ["o1"])))
        (is (= ["test3" "a" "o2"] (rslv g ["test3" "a"] ["o2"])))
        (is (= ["test3" "a" "o1"] (rslv g ["test3" "a" "o1"] ["o1"])))
        (is (= ["test3" "a" "o2"] (rslv g ["test3" "a" "o1"] ["o2"])))
        (is (= ["test3" "a" "o1"] (rslv g ["test3" "a" "o2"] ["o1"])))
        (is (= ["test3" "a" "o2"] (rslv g ["test3" "a" "o2"] ["o2"]))))))

  (testing "Classes with inheritance 4, shadowing."
    (with-redefs [fixture {:derived {["test4" "a"] nil
                                     ["test4" "a" "i1"] nil
                                     ["test4" "a" "i2"] nil
                                     ["test4" "b"] ["a"]
                                     ["test4" "b" "i1"] nil
                                     ["test4" "b" "i3"] nil
                                     ["test4" "c"] ["b"]
                                     ["test4" "c" "x1"] ["i1"]
                                     ["test4" "c" "x2"] ["i2"]
                                     ["test4" "c" "x3"] ["i3"]}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-edge? g ["test4" "b"] ["test4" "a"]))
        (is (uber/has-edge? g ["test4" "c"] ["test4" "b"]))
        (is (uber/has-edge? g ["test4" "c" "x1"] ["test4" "b" "i1"]))
        (is (uber/has-edge? g ["test4" "c" "x2"] ["test4" "a" "i2"]))
        (is (uber/has-edge? g ["test4" "c" "x3"] ["test4" "b" "i3"]))
        (is (= 12 (count (uber/nodes g))) "number of nodes")
        (is (= 5 (count (uber/edges g))) "number of edges"))))

  
  (testing "Classes with inheritance 5, outer view (special)"
    (with-redefs [fixture {:derived {["test5" "w"] nil
                                     ["test5" "w" "i0"] nil
                                     ["test5" "a"] ["w"]
                                     ["test5" "a" "i1"] nil
                                     ["test5" "a" "i2"] nil
                                     ["test5" "b"] nil
                                     ["test5" "b" "i3"] ["a" "i1"]
                                     ["test5" "c"] ["b" "i3"]
                                     ["test5" "c" "i4"] ["i2"]
                                     ["test5" "d"] ["a" "i2"]
                                     ["test5" "d" "i5"] ["i1"]
                                     ["test5" "d" "i6"] ["i0"]}}]  ;; not resolvable
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-edge? g ["test5" "a"] ["test5" "w"]))
        (is (uber/has-edge? g ["test5" "b" "i3"] ["test5" "a" "i1"]))
        (is (uber/has-edge? g ["test5" "c"] ["test5" "b" "i3"]))
        (is (uber/has-edge? g ["test5" "c" "i4"] ["test5" "a" "i2"]))
        (is (uber/has-edge? g ["test5" "d"] ["test5" "a" "i2"]))
        (is (uber/has-edge? g ["test5" "d" "i5"] ["test5" "a" "i1"]))
        (is (not (uber/has-edge? g ["test5" "d2" "i6"] ["test5" "a" "i0"])))
        (is (= 13 (count (uber/nodes g))) "number of nodes")
        (is (= 6 (count (uber/edges g))) "number of edges")

        (is (nil? (rslv g ["test5" "d"] ["i0"])))
        (is (= ["test5" "a" "i1"] (rslv g ["test5" "d"] ["i1"])))
        (is (= ["test5" "a" "i2"] (rslv g ["test5" "d"] ["i2"])))
        (is (= ["test5" "d" "i5"] (rslv g ["test5" "d"] ["i5"])))
        (is (nil? (rslv g ["test5" "d" "i5"] ["i0"])))
        (is (= ["test5" "a" "i1"] (rslv g ["test5" "d" "i5"] ["i1"])))
        (is (= ["test5" "a" "i2"] (rslv g ["test5" "d" "i5"] ["i2"])))
        (is (= ["test5" "d" "i5"] (rslv g ["test5" "d" "i5"] ["i5"])))
        (is (= ["test5" "a" "i1"] (rslv g ["test5" "c" "i4"] ["i1"])))
        (is (= ["test5" "a" "i2"] (rslv g ["test5" "c" "i4"] ["i2"])))
        (is (= ["test5" "b" "i3"] (rslv g ["test5" "c" "i4"] ["i3"])))
        (is (= ["test5" "c" "i4"] (rslv g ["test5" "c" "i4"] ["i4"])))
        (is (nil? (rslv g ["test5" "c" "i4"] ["i0"])))
        (is (nil? (rslv g ["test5" "c" "i4"] ["i5"]))))))

  (testing "Classes with inheritance 6, outer view b (special)"
    (with-redefs [fixture {:derived {["test6" "a"] nil
                                     ["test6" "a" "i1"] nil
                                     ["test6" "a" "p1"] nil
                                     ["test6" "a" "q1"] nil
                                     ["test6" "b"] nil
                                     ["test6" "b" "i2"] ["a" "i1"]
                                     ["test6" "b" "p2"] nil
                                     ["test6" "b" "q1"] nil
                                     ["test6" "c"] ["b" "i2"]
                                     ["test6" "c" "x1"] ["p1"]
                                     ["test6" "c" "x2"] ["p2"]
                                     ["test6" "c" "x3"] ["q1"]}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-edge? g ["test6" "b" "i2"] ["test6" "a" "i1"]))
        (is (uber/has-edge? g ["test6" "c"] ["test6" "b" "i2"]))
        (is (uber/has-edge? g ["test6" "c" "x1"] ["test6" "a" "p1"]))
        (is (uber/has-edge? g ["test6" "c" "x2"] ["test6" "b" "p2"]))
        (is (uber/has-edge? g ["test6" "c" "x3"] ["test6" "b" "q1"]))
        (is (= 14 (count (uber/nodes g))) "number of nodes")
        (is (= 5 (count (uber/edges g))) "number of edges")
        (is (= ["test6" "a" "i1"] (rslv g ["test6" "c"] ["i1"])))
        (is (= ["test6" "a" "p1"] (rslv g ["test6" "c"] ["p1"])))
        (is (= ["test6" "b" "q1"] (rslv g ["test6" "c"] ["q1"])))
        (is (= ["test6" "b" "i2"] (rslv g ["test6" "c"] ["i2"])))
        (is (= ["test6" "b" "p2"] (rslv g ["test6" "c"] ["p2"])))
        (is (= ["test6" "a" "i1"] (rslv g ["test6" "c" "x1"] ["i1"])))
        (is (= ["test6" "a" "p1"] (rslv g ["test6" "c" "x1"] ["p1"])))
        (is (= ["test6" "a" "q1"] (rslv g ["test6" "c" "x1"] ["q1"])))
        (is (= ["test6" "b" "i2"] (rslv g ["test6" "c" "x1"] ["i2"])))
        (is (= ["test6" "b" "p2"] (rslv g ["test6" "c" "x1"] ["p2"])))
        (is (= ["test6" "a" "i1"] (rslv g ["test6" "c" "x2"] ["i1"])))
        (is (= ["test6" "a" "p1"] (rslv g ["test6" "c" "x2"] ["p1"])))
        (is (= ["test6" "b" "q1"] (rslv g ["test6" "c" "x2"] ["q1"])))
        (is (= ["test6" "b" "i2"] (rslv g ["test6" "c" "x2"] ["i2"])))
        (is (= ["test6" "b" "p2"] (rslv g ["test6" "c" "x2"] ["p2"])))
        (is (= ["test6" "a" "i1"] (rslv g ["test6" "c" "x3"] ["i1"])))
        (is (= ["test6" "a" "p1"] (rslv g ["test6" "c" "x3"] ["p1"])))
        (is (= ["test6" "b" "q1"] (rslv g ["test6" "c" "x3"] ["q1"])))
        (is (= ["test6" "b" "i2"] (rslv g ["test6" "c" "x3"] ["i2"])))
        (is (= ["test6" "b" "p2"] (rslv g ["test6" "c" "x3"] ["p2"]))))))

  (testing "Classes with inheritance 7, outer view (special+shadowing)"
    (with-redefs [fixture {:derived {["test7" "a"] nil
                                     ["test7" "a" "i1"] nil
                                     ["test7" "a" "i2"] nil
                                     ["test7" "a" "i3"] nil
                                     ["test7" "b"] nil
                                     ["test7" "b" "j1"] ["a" "i1"]
                                     ["test7" "b" "i2"] nil
                                     ["test7" "c"] ["b" "j1"]
                                     ["test7" "c" "x1"] ["i2"]
                                     ["test7" "c" "x2"] ["i3"]}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-edge? g ["test7" "b" "j1"] ["test7" "a" "i1"]))
        (is (uber/has-edge? g ["test7" "c"] ["test7" "b" "j1"]))
        (is (uber/has-edge? g ["test7" "c" "x1"] ["test7" "b" "i2"]))
        (is (uber/has-edge? g ["test7" "c" "x2"] ["test7" "a" "i3"]))
        (is (= 12 (count (uber/nodes g))) "number of nodes")
        (is (= 4 (count (uber/edges g))) "number of edges")
        (is (= ["test7" "a" "i1"] (rslv g ["test7" "c"] ["i1"])))
        (is (= ["test7" "b" "i2"] (rslv g ["test7" "c"] ["i2"])))
        (is (= ["test7" "a" "i3"] (rslv g ["test7" "c"] ["i3"])))
        (is (= ["test7" "b" "j1"] (rslv g ["test7" "c"] ["j1"])))
        (is (= ["test7" "a" "i1"] (rslv g ["test7" "c" "x1"] ["i1"])))
        (is (= ["test7" "b" "i2"] (rslv g ["test7" "c" "x1"] ["i2"])))
        (is (= ["test7" "a" "i3"] (rslv g ["test7" "c" "x1"] ["i3"])))
        (is (= ["test7" "b" "j1"] (rslv g ["test7" "c" "x1"] ["j1"])))
        (is (= ["test7" "a" "i1"] (rslv g ["test7" "c" "x2"] ["i1"])))
        (is (= ["test7" "a" "i2"] (rslv g ["test7" "c" "x2"] ["i2"])))
        (is (= ["test7" "a" "i3"] (rslv g ["test7" "c" "x2"] ["i3"])))
        (is (= ["test7" "b" "j1"] (rslv g ["test7" "c" "x2"] ["j1"]))))))

  (testing "Classes with inheritance 8, no inner leak"
    (with-redefs [fixture {:derived {["test8" "a"] nil
                                     ["test8" "a" "i1"] nil
                                     ["test8" "b"] nil
                                     ["test8" "b" "i2"] ["a"]
                                     ["test8" "b" "i3"] ["i1"]}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-edge? g ["test8" "b" "i2"] ["test8" "a"]))
        (is (not (uber/has-edge? g ["test8" "b" "i3"] ["test8" "a" "i1"])))
        (is (= 6 (count (uber/nodes g))) "number of nodes")
        (is (= 1 (count (uber/edges g))) "number of edges")
        (is (nil? (rslv g ["test8" "b"] ["i1"])))
        (is (nil? (rslv g ["test8" "b"] ["i3"])))
        (is (= ["test8" "b" "i2"] (rslv g ["test8" "b"] ["i2"])))
        (is (= ["test8" "a" "i1"] (rslv g ["test8" "b" "i2"] ["i1"])))
        (is (= ["test8" "b" "i2"] (rslv g ["test8" "b" "i2"] ["i2"])))
        (is (nil? (rslv g ["test8" "b" "i2"] ["i3"]))))))

  (testing "Classes with inheritance 9, chain"
    (with-redefs [fixture {:derived {["test9" "a"] nil
                                     ["test9" "a" "foo"] nil
                                     ["test9" "a" "qux"] nil
                                     ["test9" "b"] ["a" "foo"]
                                     ["test9" "b" "bar"] nil
                                     ["test9" "c"] ["b" "bar"]}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-edge? g ["test9" "b"] ["test9" "a" "foo"]))
        (is (uber/has-edge? g ["test9" "c"] ["test9" "b" "bar"]))
        (is (= 8 (count (uber/nodes g))) "number of nodes")
        (is (= 2 (count (uber/edges g))) "number of edges")
        (is (nil? (rslv g ["test9" "c"] ["foo"])))
        (is (nil? (rslv g ["test9" "c"] ["qux"])))
        (is (= ["test9" "a" "foo"] (rslv g ["test9" "b" "bar"] ["foo"])))
        (is (= ["test9" "a" "qux"] (rslv g ["test9" "b" "bar"] ["qux"])))
        (is (= ["test9" "a" "foo"] (rslv g ["test9" "b"] ["foo"])))
        (is (= ["test9" "a" "qux"] (rslv g ["test9" "b"] ["qux"]))))))

  (testing "Classes with circular inheritance 1"
    (with-redefs [fixture {:derived {["test9" "a"] ["b"]
                                     ["test9" "b"] ["a"]
                                     ["test9" "c"] ["a"]}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (= 2 (count (uber/nodes g))) "number of nodes")
        (is (= 0 (count (uber/edges g))) "number of edges"))))

  (testing "Classes across packages"
    (with-redefs [fixture {:derived {["foobar" "o1"] nil
                                     ["foobar" "o1" "i1"] nil
                                     ["foobar" "o1" "claire"] nil
                                     ["foobar" "o1" "pedro"] nil
                                     ["foobar" "o2"] nil
                                     ["foobar" "o2" "i2"] ["o1" "i1"]
                                     ["foobar" "o2" "pedro"] nil
                                     ["test10" "Ext"] ["foobar" "o2" "i2"]}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-edge? g ["foobar" "o2" "i2"] ["foobar" "o1" "i1"]))
        (is (uber/has-edge? g ["test10" "Ext"] ["foobar" "o2" "i2"]))
        (is (= 10 (count (uber/nodes g))) "number of nodes")
        (is (= 2 (count (uber/edges g))) "number of edges")
        (is (= ["foobar" "o1"] (rslv g ["test10"] ["foobar" "o1"])))
        (is (= ["foobar" "o1" "i1"] (rslv g ["test10"] ["foobar" "o1" "i1"])))
        (is (= ["foobar" "o1" "i1"] (rslv g ["test10" "Ext"] ["i1"])))
        (is (= ["foobar" "o2" "i2"] (rslv g ["test10" "Ext"] ["i2"])))
        (is (= ["foobar" "o1" "claire"] (rslv g ["test10" "Ext"] ["claire"])))
        (is (= ["foobar" "o2" "pedro"] (rslv g ["test10" "Ext"] ["pedro"])))
        (is (nil? (rslv g ["test10" "Ext"] ["o1"])))
        (is (nil? (rslv g ["test10" "Ext"] ["o2"]))))))
  
  (testing "Classes with inheritance 11, no shadowing"
    (with-redefs [fixture {:derived {["test11" "a"] nil
                                     ["test11" "a" "b"] nil
                                     ["test11" "a" "c"] nil
                                     ["test11" "b"] nil
                                     ["test11" "d"] ["a"]}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-edge? g ["test11" "d"] ["test11" "a"]))
        (is (= 7 (count (uber/nodes g))) "number of nodes")
        (is (= 1 (count (uber/edges g))) "number of edges")
        (is (= ["test11" "a"] (rslv g ["test11" "d"] ["a"])))
        (is (= ["test11" "a" "b"] (rslv g ["test11" "d"] ["b"])))
        (is (= ["test11" "a" "c"] (rslv g ["test11" "d"] ["c"])))
        (is (= ["test11" "d"] (rslv g ["test11" "d"] ["d"]))))))

  (testing "Classes with inheritance 12, shadowing"
    (with-redefs [fixture {:derived {["test12" "a"] nil
                                     ["test12" "a" "competing"] nil
                                     ["test12" "a" "competing2"] nil 
                                     ["test12" "b"] nil
                                     ["test12" "b" "competing"] nil
                                     ["test12" "ext"] ["a"]
                                     ["test12" "ext" "inext"] ["b"]}}]
      (let [g (calculate-inheritance-graph-only-classes)]
        (is (uber/has-edge? g ["test12" "ext"] ["test12" "a"]))
        (is (uber/has-edge? g ["test12" "ext" "inext"] ["test12" "b"]))
        (is (= 9 (count (uber/nodes g))) "number of nodes")
        (is (= 2 (count (uber/edges g))) "number of edges")
        (is (nil? (rslv g ["test12" "nonexistant"] ["a"])))
        (is (nil? (rslv g ["foo"] ["a"])))
        (is (= ["test12" "a"] (rslv g ["test12"] ["a"])))
        (is (= ["test12" "ext" "inext"] (rslv g ["test12"] ["ext" "inext"])))
        (is (= ["test12" "a" "competing"] (rslv g ["test12" "ext"] ["competing"])))
        (is (= ["test12" "a" "competing2"] (rslv g ["test12" "ext"] ["competing2"])))
        (is (= ["test12" "b" "competing"] (rslv g ["test12" "ext" "inext"] ["competing"])))
        (is (= ["test12" "a" "competing2"] (rslv g ["test12" "ext" "inext"] ["competing2"])))))))

