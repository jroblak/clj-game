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
; note, to import records, have to IMPORT them separately since they're JAVA objects
(defrecord MyType [a b])
(def foo (->MyType [1 2 3] [4 5 6]))
foo

(defn mytype-with-length [n]
  (let [a (vec (range n))
        b (vec (range n))]
    (->MyType a b)))

(mytype-with-length 4)
(assoc foo :b [7,8,9,10])

(defrecord Person [fname lname address])
(defrecord Address [street city state zip])

; can use similar fn as with maps ->, assoc, update-in
; note the "." defrecords are Java objects (still)
(def stu (Person. "Stu" "Halloway"
           (Address. "200 N Mangum"
                      "Durham"
                      "NC"
                       "12345")))
(:lname stu)
(-> stu :address :city)


; FUNCTIONS (http://java.ociweb.com/mark/clojure/article.html#DefiningFunctions)
; ---------

(defn parting ; (defn- functionnname) creates function visible only in that namespace
  "returns a String parting" ; optional descriptor
  [name]      ; function parameters
  (str "Goodbye, " name)) ; function body
(println (parting "Mark"))

(defn power [base & exponents] ; optional parameters in list after &
  ; Using java.lang.Math static method pow.
  (reduce #(Math/pow %1 %2) base exponents))

; (declare function-names) - a way to create forward declarations of functions
; like Haskell, Clojure functions can pattern match-ish:
(defn parting
  "returns a String parting in a given language"
  ([] (parting "World"))
  ([name] (parting name "en"))
  ([name language]
    ; condp is similar to a case statement in other languages.
    ; It is described in more detail later.
    ; It is used here to take different actions based on whether the
    ; parameter "language" is set to "en", "es" or something else.
    (condp = language
      "en" (str "Goodbye, " name)
      "es" (str "Adios, " name)
      (throw (IllegalArgumentException.
        (str "unsupported language " language))))))
(println (parting))
(println (parting "Mark"))
(println (parting "Mark" "es"))
; anon functions:
(#(even? %1) 2)
(#(even? %) 2)
(filter (fn anon [arg] (even? arg)) [1940 1941]) ; named anon fn (can be called recursively)

; defmulti and defmethod create multimethods
; (defmulti method-name dispatch-fn)
; (defmethod method-name dispatch-value-that-triggers-method [parameter-list] body)
(defmulti what-am-i class) ; class is the dispatch function
; returns the class of the argument to the method
(defmethod what-am-i Number [arg] (println arg "is a Number"))
; takes what was returned from def multi, parameter matches on it,
; and takes the arg(s) and performs something on them
(defmethod what-am-i String [arg] (println arg "is a String"))
(defmethod what-am-i :default [arg] (println arg "is something else"))
(what-am-i 19) ; -> 19 is a Number
(what-am-i "Hello") ; -> Hello is a String
(what-am-i true) ; -> true is something else
; same-ish thing without defmulti / defmethod
(defn callback1 [n1 n2 n3] (+ n1 n2 n3))
(defn callback2 [n1 _ n3] (+ n1 n3))
(defn caller [callback value]
  (callback (+ value 1) (+ value 2) (+ value 3)))
(caller callback1 10)
(caller callback2 10)
; compliment takes a function and returns the opposite boolean value
; comp is function composition
(defn times2 [n] (* n 2))
(defn minus3 [n] (- n 3))
; Note the use of def instead of defn because comp returns
; a function that is then bound to "my-composition".
(def my-composition (comp minus3 times2)) ; == minus3 . times2
(my-composition 4)
; partial performs partial application
(def times2 (partial * 2)) ;
(times2 3 4) ; 2 * 3 * 4 ->
(times2 3 4 5) ; 2 * 3 * 4 * 5

(defn- polynomial
  [coefs x]
  (let [exponents (reverse (range (count coefs)))] ; range generates lazy list from 0 to count coefs
    (apply + (map #(* %1 (Math/pow x %2)) coefs exponents))))

(defn- derivative
  [coefs x]
  (let [exponents (reverse (range (count coefs)))
        derivative-coefs (map #(* %1 %2) (butlast coefs) exponents)]
    (polynomial derivative-coefs x)))

(polynomial [2 3 1] 3) ; 2x^2 + 3x + 1
(derivative [2 3 1] 3) ; 4x + 3
(def f (partial polynomial [2 1 3])) ; 2x^2 + x + 3
(def f-prime (partial derivative [2 1 3])) ; 4x + 1

; Note the use of def instead of defn because memoize returns
; a function that is then bound to "memo-f".
; memoize returns a function that stores a mapping from previous args to previous results
; the new fn uses that mapping to avoid invoking the given function with args that have already
; been eval'd. this gives better perf but worse memory use
(def memo-f (memoize f))

(println "priming call")
(time (f 2)) ; time just evals the function and prints the time as well as outputting result

(println "without memoization")
; Note the use of an underscore for the binding that isn't used.
; dotimes = for loop?
(dotimes [_ 3] (time (f 2)))

(println "with memoization")
(dotimes [_ 3] (time (memo-f 2)))

; BELOW commented out because has overhead / annoying stuff
; clj can use all java classes and interfaces
;(import
; '(java.util Calendar GregorianCalendar)
; '(javax.swing JFrame JLabel))
;(. java.util.Calendar APRIL)
;(. Calendar APRIL) ; works if we imported Calendar
;(. Math pow 2 4)
;(Math/pow 2 4) ; both are valid ways of calling static fns
;(def calendar (new GregorianCalendar 2008 Calendar/APRIL 16))
;(def calendar (GregorianCalendar. 2008 Calendar/APRIL 16))
;(. calendar add Calendar/MONTH 2)
;(. calendar get Calendar/MONTH)
;(. (. calendar getTimeZone) getDisplayName) ; long way
;(.. calendar getTimeZone getDisplayName)
; doto invokes many methods on same object and returns the value of its first
; argument which is the target object
; this means its smart to init the object in the first part of the call
;(doto (JFrame. "Hello")
;  (.add (JLabel. "Hello, World!"))
;  (.pack)
;  (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
;  (.setVisible true))
; can also use .?. instead of .. it returns null if anything in the chain returns null
; memfn expands to code that allows a java method to be a first class function
(map #(.substring %1 %2)
           ["Moe" "Larry" "Curly"] [1 2 3])
(map (memfn substring beginIndex)
           ["Moe" "Larry" "Curly"] [1 2 3])

; proxy macro expands ccode that creates a java object that extends a given java class
; or implements zero or more java interfaces. more on this later.

; THREADS
(defn delayed-print [ms text]
  (Thread/sleep ms)
  (println text))
; starts a new thread, passing the delayed-print to it as a lambda
;(.start (Thread. #(delayed-print 1000 ", World")))
;(print "Hello")

; clj implements throw / try / catch / finally:
(defn collection? [obj]
  (println "obj is a" (class obj))
  ; Clojure collections implement clojure.lang.IPersistentCollection.
  (or (coll? obj) ; Clojure collection?
      (instance? java.util.Collection obj))) ; Java collection?

(defn average [coll]
  (when-not (collection? coll)
    (throw (IllegalArgumentException. "expected a collection")))
  (when (empty? coll)
    (throw (IllegalArgumentException. "collection is empty")))
  ; Apply the + function to all the items in coll,
  ; then divide by the number of items in it.
  (let [sum (apply + coll)]
    (/ sum (count coll))))

(try
  (println "list average =" (average '(2 3))) ; result is a clojure.lang.Ratio object
  (println "vector average =" (average [2 3])) ; same
  (println "set average =" (average #{2 3})) ; same
  (let [al (java.util.ArrayList.)]
    (doto al (.add 2) (.add 3))
    (println "ArrayList average =" (average al))) ; same
  (println "string average =" (average "1 2 3 4")) ; illegal argument
  (catch IllegalArgumentException e
    (println e)
    ;(.printStackTrace e) ; if a stack trace is desired
  )
  (finally
    (println "in finally")))

; clj also has if / when / when-not
; if-let does an assignment and then executes depending on whether that assignment value
; is logically true for false
(defn process-next [waiting-line]
  (if-let [name (first waiting-line)]
    (println name "is next")
    (println "no waiting")))

(process-next '("Jeremy")) ; -> Jeremy is next
(process-next '()) ; -> no waiting
; when-let is similar - basically no else but unlimited body expressions
(defn summarize
  "prints the first item in a collection
  followed by a period for each remaining item"
  [coll]
  ; Execute the when-let body only if the collection isn't empty.
  (when-let [head (first coll)]
    (print head)
    ; Below, dec subtracts one (decrements) from
    ; the number of items in the collection.
    (dotimes [_ (dec (count coll))] (print \.))
    (println)))

(summarize ["Moe" "Larry" "Curly"]) ; -> Moe..
(summarize ["Moe" "Curly"]) ; -> Moe.
(summarize []) ; -> no output

; condp = case
(def value 2)
(condp = value
  1 "one"
  2 "two"
  3 "three"
  (str "unexpected value, \"" value \"))
; cond is similar, but uses expressions
(def temperature 10)
(cond
 (instance? String temperature) "invalid temperature"
 (<= temperature 0) "freezing"
 (>= temperature 100) "boiling"
 true "neither") ; this is like "otherwise" in Haskell

; ITERATION
; clj has dotimes
(dotimes [card-number 3]
  (println "deal card number" (inc card-number)))
; while (while (test) (do something))

; LIST COMPREHENSION
(def cols "ABCD")
(def rows (range 1 4))
(dorun
 (for [col cols :when (not= col \B)
       row rows :while (< row 3)]
   (println (str col row))))
(doseq [col cols :when (not= col \B)
        row rows :while (< row 3)]
  (println (str col row)))
; clj also has loop and recur

; RECURSION
; java and clj do not support tail call optimization, so we need to use recur and loop special forms
(defn factorial-1 [number]
  "computes the factorial of a positive integer
   in a way that doesn't consume stack space"
  (loop [n number factorial 1]
    (if (zero? n)
      factorial
      (recur (dec n) (* factorial n)))))
; defn and loop establish "recursion points"

; PREDICATES
; reflection: class?, coll?, decimal?, delay?, float?, fn?, instance?, integer?, isa?, keyword?, list?, macro?, map?, number?, seq?, set?, string? and vector?
; non-predicate functions that perform reflection: ancestors, bases, class, ns-publics and parents
; test relationships between values: <, <=, =, not=, ==, >, >=, compare, distinct? and identical?
; test logical relationships: and, or, not, true?, false? and nil?
; test sequences: empty?, not-empty, every?, not-every?, some and not-any?
; test numbers: even?, neg?, odd?, pos? and zero?

; SEQUENCES
; are logical views of collections
; almost everything is a sequence, and they can be lazy
; not that lazy sequences are not evaluated when run, so (map #(println %) [1 2 3])
; will do nothing (since it just creates a list of things to be evald)
; in order to eval, one needs to extract a single items like first, second, nth, last
; or use doall, doseq, and dorun
; doseq and dorun are apropriate when the goal is simply to cause the side effects to occur
; both return nil (doseq is typically faster and easier to read so its preferred)
; doall is used when the eval results need to be retained.
(dorun (map #(println %) [1 2 3]))
(doseq [i [1 2 3]] (println i))
(doall (map #(do (println %) %) [1 2 3]))
; below creates an infinite seq
(def f-seq (map f (iterate inc 0)))
(nth f-seq 10)
(first f-seq)
; vs non-binding:
(defn f-seq [] (map f (iterate inc 0)))
(println (first (f-seq))) ; evaluates (f 0), but doesn't cache result

; INPUT / OUTPUT
;  *in*, *out* and *err* are set to stdin, stdout and stderr by default.
; can rebind these ala: (binding [*out* (java.io.FileWriter. "my.log")] (println "This goes to the file my.log."))
; print-str, println-str are the same has print, et al but they return the output as a string, not to stdout
; printf for format strings
; with-open great for streams / databases
; line-seq takes a java.io.BufferedReader and returns lazy seq of lines of text in it
;(use '[clojure.java.io :only (reader)])

;(defn print-if-contains [line word]
;  (when (.contains line word) (println line)))

;(let [file "story.txt"
;      word "fur"]
  ; with-open will close the reader after
  ; evaluating all the expressions in its body.
;  (with-open [rdr (reader file)]
;    (doseq [line (line-seq rdr)] (print-if-contains line word))))
; slurps reads entire file to string and returns it, spit writes a string to a file and closes

; DESTRUCTURING (http://java.ociweb.com/mark/clojure/article.html#Destructuring)
; pattern matching (Haskell)
(defn approach2 [[n1 _ n3]] (+ n1 n3))
(defn name-summary [[name1 name2 & others]] ; == name-summary (name1:name2:others)
  (println (str name1 ", " name2) "and" (count others) "others"))
; :as can be used to retain access to the whole collection (like @ in haskell)
(defn first-and-third-percentage [[n1 _ n3 :as coll]] ; == first-and-third-percentage coll@(x:_:y) =
  (/ (+ n1 n3) (apply + coll)))
; can also pattern match maps by key

; NAMESPACES
; usually declared at top of file
; can require, use, import, or refer to NAMESPACES
; require just loads, still need to use fully qualified name
; refer makes all symbols in namesapce accessible (so no fully qualified names)
; use combines require and refer
; import is for java imports
; ns actually declares the namespace:
;(ns com.ociweb.demo
;  (:require [clojure.string :as su])
;  ; assumes this dependency: [org.clojure/math.numeric-tower "0.0.1"]
;  (:use [clojure.math.numeric-tower :only (gcd, sqrt)])
;  (:import (java.text NumberFormat) (javax.swing JFrame JLabel)))

; def creates symbol in default ns
; create-ns makes ns but doesnt make it default
; intern creates symbol in non-default ns
(def foo 1)
(create-ns 'com.ociweb.demo)
(intern 'com.ociweb.demo 'foo 2)
(println (+ foo com.ociweb.demo/foo))
; ns-interns returns a map containing all symbols defined in a given ns

; METADATA
(defrecord card [rank suit])
(def card1 (->card :king :club))
(def card2 (->card :king :club))
(= card1 card2)
(def card2 ^{:bent true} card2)
(def card2 (with-meta card2 {:bent true})) ; adds meta at Runtime
(meta card1)
(meta card2)
(= card1 card2) ; meta doesnt change equality

; MACROS
; code that generates code at read-time
; to determine if something is a macro:
(meta #'and)

; how to define macro
(defmacro around-zero [number negative-expr zero-expr positive-expr] ; so far just like a fn
  `(let [number# ~number] ; so number is only evaluated once, enables hygienic macros
                          ; prefix# = auto-gensym which generates unique symbol names
 ;^--- backtick prevents everything inside from being eval'd unless it is unquoted
 ; this means contents will appear literally in the expansion, except items preceded by tilde
 ; when it has a tilde, the value is substituted
    (cond
      (< (Math/abs number#) 1e-15) ~zero-expr
      (pos? number#) ~positive-expr
      true ~negative-expr)))

; CONCURRENCY
; future runs a body of expressions using a thread in the thread pool with an agent (see later)
; useful for long-running expressions whose results arent needed immediately. result obtained by
; derefing. if not ready when deref'd, it locks until it is
(def my-future (future (f-prime 2))) ; f-prime is called in another thread
(println "created future")
(println "result is" @my-future)
(shutdown-agents)

; pmap applies a function to all items in a collection in parrallel. better perf than map
; when fn being applied is time consuming compared to overhead of managing threads
; clojure.parallel has many helpful fns: par, pdistinct, pfilter-dupes, pfilter-nils, pmax, pmin, preduce, psort, psummary and pvec.

; REFERENCE TYPES
; ref types rae mutable refs to immutable values
; four ref types: Var, Ref, Atom, Agent
; all four can
;   - hold any object
;   - be deref'd to retrieve obj they hold (w/ deref or @ macro)
;   - support validators (fns invoked when value changes)
;   - support watchers (agents). an agent is notified when a value changes

; VAR: synchronous changes (def name value); to mod (def name new-val)
; (alter-var-root (var name) update-fn args) - atomically set new val
; ---------------------------------------------------------------------
; (set! name new-val) / (binding [name exprs] body) - set new thread-local value inside binding form
; VARs are references that can have a root binding that is shared by all threads and can have
; a different value in each thread (thread-local)


; REF: synchronous, coordinated changes (ref init-value); to mod (ref-set ref new-val)
; (alter ref update-fn args) - must be inside dosync; (commute ref update-fn args) - must be in dosync
; ----------------------------------------------------------------------
; REFS ensure changes to one or more bindings are coordinated between multiple threads
; this is done via STM (Software Transactional Memory). Refs can only be modified inside a
; transaction (dosync) - similar to database transactions
(def aname (ref 1))
(dosync
 (ref-set aname 2))
; commute used for changes whose order is not important
; alter is used for changes whose order is
; both alter and commute are better than ref-set when the new value depends on the old
; commute is faster, but harder to use

; ATOM: synchronous changes to a single value (atom init-value); to mod (reset! atmo new-val)
; (compare-and-set! atom current-value new-value); (swap! atmo update-fn args)
; ----------------------------------------------------------------------
(def my-atom (atom 1))
(reset! my-atom 2)
(println @my-atom)
; compare-and-set! takes what is believe the current value is, a new value, and if the current value
; is correct, it updates it and true is returned
(def my-atom (atom 1))

(defn update-atom []
  (let [curr-val @my-atom]
    (println "update-atom: curr-val =" curr-val) ; -> 1
    (Thread/sleep 50) ; give reset! time to run
    (println
      (compare-and-set! my-atom curr-val (inc curr-val))))) ; -> false

(let [thread (Thread. #(update-atom))]
  (.start thread)
  (Thread/sleep 25) ; give thread time to call update-atom
  (reset! my-atom 3) ; happens after update-atom binds curr-val
  (.join thread)) ; wait for thread to finish

(println @my-atom) ; -> 3

; swap! takes atom to be set, fn to new value and any args. the fn is called with the current val
; of the atom and the args. its basically a wrapper around compare-and-set!, BUT it begins by deref'ing
; the atoms to save its current value. next, it computes new value using fn. finally, it calls
; compare-and-set using value obtained at the beginning. IF THE CHECK FAILS, THIS IS REPEATED UNTIL
; THE CHECK DOES NOT FAIL
(def my-atom (atom 1))

(defn update-atom [curr-val]
  (println "update-atom: curr-val =" curr-val)
  (Thread/sleep 50) ; give reset! time to run
  (inc curr-val))

(let [thread (Thread. #(swap! my-atom update-atom))]
  (.start thread)
  (Thread/sleep 25) ; give swap! time to call update-atom
  (reset! my-atom 3)
  (.join thread)) ; wait for thread to finish

(println @my-atom) ; -> 4

; AGENT: asynchronous changes to a single value (agent init-val); to mod (send agent update-fn args)
; (send-off agent update-fn args)
; ----------------------------------------------------------------------
; agents are used to run tasks in separate threads that typically dont need coordination.
; useful for modifying state of a single object in the agent - done by running an "action" (fn)
; on a separate thread that changes the value. ONLY ONE ACTION AT A TIME WILL BE RUN PER AGENT
(def my-agent (agent 2))
; send dispatches an action to an Agent and returns immediately instead of waiting for action
; to finish.
; send-off is similar, but uses threads from a different pool (fixed thread pool vs cached)
; agents / agents sent in transactions arent sent until transaction completes
; inside actions, the agent on which they are operating are bound to *agent*
; await takes any number of agents and blocks the current thread until they all complete.
; await-for is similar, but takes a timeout as well. if fn doesnt return before timeout, nil
; is returned. neither await nor await-for can be used in a transaction
; shutdown-agents waits for the execution of all actions sent to agents to complete. it then stops all
; threads in the thread pools that are used by agents. allows JVM to exit properly

; -- WATCHERS
; looks for different tutorial on add-watch and remove-watch

; COMPILING
;When Clojure source files are executed as scripts, they are compiled to Java bytecode at runtime. They can also be compiled to Java bytecode ahead of time (AOT). This improves the startup time of Clojure applications and produces .class files that can be used in Java applications. Recommended steps to do this are:

;Select a namespace for the source files to be compiled, for example, com.ociweb.talk.
;Create directories named "src" and "classes" in the same parent directory.
;Make one of the source files have the same name as the last part of the namespace. We'll call this the main source file. For example, talk.clj.
;Place the source files under the "src" directory in a directory structure patterned after the namespace. For example, the main source would be src/com/ociweb/talk.clj.
;Specify the namespace at the top of the main source file and include the :gen-class namespace directive. For example: (ns com.ociweb.talk (:gen-class))
;In the main source file, use the load function to load all the other source files in the same namespace with relative paths. For example, if the file more.clj is in a subdirectory of src/com/ociweb named "talk", use (load "talk/more").
;In each of the other source files, use the in-ns function to set their namespace. For example, add (in-ns 'com.ociweb.talk) to the top of more.clj.
;Add the "src" and "classes" directories to the classpath used by the REPL. If a script is used to run the REPL, modify that script.
;Start a REPL.
;Use the compile function to compile all the source files in a given namespace by entering (compile 'namespace). For example: (compile 'com.ociweb.talk).
;A separate .class file is produced for each function. They are written under the "classes" directory in a directory structure that corresponds to their namespace.

;If the compiled namespace has a function named -main, it can be run as a Java application. Command-line arguments are passed as arguments to that function. For example, if talk.clj contained a -main function, it could be run as follows:

;java -classpath path/classes:path/clojure.jar com.ociweb.talk args

; TESTING
; tests can be written using macros defined in in clojure.test
(use 'clojure.test)
(deftest add-test
  ; The "is" macro takes a predicate, arguments to it,
  ; and an optional message.
  (is (= 4 (+ 2 2)))
  (is (= 2 (+ 2 0)) "adding zero doesn't change value"))

; Tests can verify that a specific exception is thrown.
(deftest division-test
  (is (thrown? ArithmeticException (/ 3.0 0))))

(with-test
  (defn my-add [n1 n2] (+ n1 n2))
  (is (= 4 (my-add 2 2)))
  (is (= 2 (my-add 2 0)) "adding zero doesn't change value"))

; The "are" macro takes a predicate template and
; multiple sets of arguments to it, but no message.
; Each set of arguments are substituted one at a time
; into the predicate template and evaluated.
(deftest multiplication
  (are [n1 n2 result]
    (= (* n1 n2) result) ; a template
    1 1 1,
    1 2 2,
    2 3 6))

(assert (>= dow 7000))

; Run all the tests in the current namespace.
; This includes tests that were added as function metadata using with-test.
; Other namespaces can be specified as quoted arguments.
(run-tests)

; can also use fixtures to set up data
;(defn fixture-name [test-function]
  ; Perform setup here.
;  (test-function)
  ; Perform teardown here.
;)

; (use-fixtures :each fixture-1 fixture-2 ...)
; OR
; (use-fixtures :once fixture-1 fixture-2 ...)
; diff is running fixtures for each test vs all tests
