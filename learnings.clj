; DATATYPES
; ----------

; all collections
(count [19 "yellow" true])
(map #(+ % 3) [2 4 7])
(apply + [1,2,3])
(def test [1 2 3])
(first test)
(nth test 2)
(last test)
(next test)

; lists (ordered collection) - ideal when items added or removed from front
; (cons > conj)
(def test2 (list 1 2 3))
(def test2 '(1 2 3))
(some #(= % 2) test2)
(some #(= % 4) test2)
(contains? (set test2) 1) ; convert list to set and use contains?
(def test2 (conj test2 "anotherthing"))
(def less-test (remove #(= % "anotherthing") test2))
(def kids-of-mike '("Greg" "Peter" "Bobby"))
(def kids-of-carol '("Marcia" "Jan" "Cindy"))
(def brady-bunch (into kids-of-mike kids-of-carol))

; vectors (ordered collection) - ideal when items added to or removed
; from the back (conj > cons)
(def test2 (vector 4 5 6))
(def test2 [4 5 6])
(get test2 1 "unknown")
(get test2 3 "unknown")
(get test2 3)
(assoc test2 2 7)
(peek test2)
(pop test2)
(subvec test2 0 2)
; plus everything above that works on lists

; sets (collection of UNIQUE items) - ideal when dupes not allowed and order
; does not matter (can be sorted or unsorted)
(def test (hash-set "Moe" "Larry" "Curly"))
(def test #{"moe" "larry" "curly"})
(contains? test "moe")
(test "larry")
(test 1)
(def more (conj test "another"))
(def less (disj more "another"))
; conj and into also work with sets as well as:
; difference, index, intersection, join, map-invert, project,
; rename, rename-keys, select and union

; maps (associative lists)
(def pop-map
  (hash-map :red :cherry, :green :apple, :purple :grape))
(def pop-map
  {:red :cherry, :green :apple, :purple :grape})
(def pop-map
  (sorted-map :red :cherry, :green :apple, :purple :grape))

(get pop-map :green)
(pop-map :green)
(:green pop-map)
(contains? pop-map :green)
(keys pop-map)
(vals pop-map)
(assoc pop-map :green :lime :blue :blueberry)
(dissoc pop-map :green :blue)
(doseq [[color flavor] pop-map]
  (println (str "The flavor of " (name color)
    " popsicles is " (name flavor) ".")))
(select-keys pop-map [:red :green :blue])
(def person {
  :name "Mark Volkmann"
  :address {
    :street "644 Glen Summit"
    :city "St. Charles"
    :state "Missouri"
    :zip 63304 }
  :employer {
    :name "Object Computing, Inc."
    :address {
      :street "12140 Woodcrest Executive Drive, Suite 250"
      :city "Creve Coeur"
      :state "Missouri"
      :zip 63141}}})
(get-in person [:employer :address :city])
(-> person :employer :address :city)
(reduce get person [:employer :address :city])
; -> is like . in Haskell
; (f1 (f2 (f3 x))) == (-> x f1 f2 f3)
; reduce == foldl in Haskell
(assoc-in person [:employer :address :city] "Clayton")
(update-in person [:employer :address :zip] str "-1234")
(str "123" "-456")

; records
; find tutorial on defrecord

; FUNCTIONS (http://java.ociweb.com/mark/clojure/article.html#DefiningFunctions)
; ---------
(defn parting
  "returns a String parting"
  [name]
  (str "Goodbye, " name))

(println (parting "Mark"))
