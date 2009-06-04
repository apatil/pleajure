;(set! *warn-on-reflection* true)
;(use 'clojure.contrib.stacktrace)

(ns pleajure)

; ====================
; = Whitespace stuff =
; ====================

(def space-tab "    ")

(defn- num-tabs [line]
    "Number of tabs at beginning of line" 
    (if (.startsWith line space-tab)
        (+ 1 (num-tabs (.substring line 4)))
        0))
    
(defn- add-tabs-before [line n]
    (if (= n 0)
        line
        (.concat space-tab (add-tabs-before line (- n 1)))))

(defn- file-lines [fname] (.split (slurp fname) "\n"))

(defn- join-line  [sym]
    "Joins lines with newlines."
    (fn
        ([] nil)
        ([line1] line1)
        ([line1 line2] (.concat (.concat line1 sym) line2))))

(defn- join-lines [lines sym] (reduce (join-line sym) lines))

(defn- add-opening-paren [line] "Adds an opening paren to line." (.concat "(" line))

(defn- add-closing-parens [line n] "Adds n closing parens to lines." 
    (if (= n 0) 
        line 
        (.concat (add-closing-parens line (- n 1)) ")")))

(defn- clean [line] "Trims whitespace from a line. Mappable." (.trim line))

(defn- replace-tail [v n] (assoc v (- (count v) 1) n))
    
(defn- translate-nextline [[out-lines cur-tab] [next-line next-tab]]
    (let [line (last out-lines)
          new-line 
            (cond
                (> next-tab cur-tab) (add-opening-paren line)
                (= next-tab cur-tab) line
                (< next-tab cur-tab) (add-closing-parens line (- cur-tab next-tab)))
      new-line (add-tabs-before new-line cur-tab)]
    [(conj (replace-tail out-lines new-line) next-line) next-tab]))

(defn- zip [s1 s2]
    " Like Python's zip"
    (map (fn [i] [(s1 i) (s2 i)]) (-> s1 count range)))

(defn- sort-line [[lines tabs shell-lines] [line tab]]
    (if (or (= (count line) 0) (.startsWith line ";"))
        [lines tabs (conj shell-lines (add-tabs-before line tab))]
        [(conj lines line) (conj tabs tab) (conj shell-lines nil)]))

(defn- first-nil [vec]
    (loop [i 0 v vec]
        (if (nil? (first v)) i (recur (+ i 1) (rest v)))))

(defn- insert-nextline [sl line] (assoc sl (first-nil sl) line))

(defn- translate-lines [lines tabs]
    "Converts trimmed lines and tab levels of a plj file to lines that form a clj file."
    (let [lines (vec lines) tabs (vec tabs)
        [lines tabs shell-lines] (reduce sort-line [[] [] []] (zip lines tabs))
        translated-lines (reduce translate-nextline [[""] 0] (zip (conj lines "") (conj tabs 0)))]
        (reduce insert-nextline shell-lines (-> 0 translated-lines rest))))
    
(defn plj-to-lines [fname]
    "Converts a plj file to the trimmed lines of a clj file."
    (let [lines (file-lines fname)
        tabs (map num-tabs lines)
        lines (map clean lines)]
        (translate-lines lines tabs)))

(defn pleajure-print [fname] 
    "Converts a plj file to the text of a clj file and prints it."
    (print (join-lines (plj-to-lines fname) "\n")))

(defn pleajure-load [fname] 
    "Converts a plj file to the text of a clj file and feeds it to the reader."
    (load-string (join-lines (plj-to-lines fname) "\n")))
    
    
; ===============
; = Infix stuff =
; ===============

; Posted by Jeff Bester to the Clojure mailing list
; Original post: http://groups.google.com/group/clojure/msg/f2bfc3aa3971ca88?dmode=source
; Thank you Jeff!

;; used for order of evaluation table and for valid infix operators 
(def +precedence+ 
     {'rem 5, 
      '* 4, 
      '/ 3, 
      '+ 2, 
      '- 1}) 
;; highest level of precedence 
(def +highest-precedence+ (apply max (map val +precedence+))) 
(defn- operator? 
  "Check if is valid operator" 
  ([sym] 
     (not (nil? (get +precedence+ sym))))) 
(defn- find-lowest-precedence 
  "find the operator with lowest precedence; search from left to 
right" 
  ([seq] 
     ;; loop through terms in the sequence 
     (loop [idx 0 
            seq seq 
            lowest-idx nil 
            lowest-prec +highest-precedence+] 
       ;; nothing left to process 
       (if (empty? seq) 
         ;; return lowest found 
         lowest-idx 
         ;; otherwise check if current term is lower 
         (let [prec (get +precedence+ (first seq))] 
           ;; is of lower or equal precedence 
           (if (and prec (<= prec lowest-prec)) 
             (recur (inc idx) (rest seq) 
                    idx prec) 
             ;; is of high precedence therefore skip for now 
             (recur (inc idx) (rest seq) 
                    lowest-idx lowest-prec))))))) 
(defn- infix-to-prefix 
  "Convert from infix notation to prefix notation" 
  ([seq] 
     (cond 
      ;; handle term only 
      (not (seq? seq)) seq 
      ;; handle sequence containing one term (i.e. handle parens) 
      (= (count seq) 1) (infix-to-prefix (first seq)) 
      ;; handle all other cases 
      true (let [lowest (find-lowest-precedence seq)] 
             (if (nil? lowest) ;; nothing to split 
               seq 
               ;; (a b c) bind a to hd, c to tl, and b to op 
               (let [[hd tl] (split-at lowest seq) 
                     op (first tl) 
                     tl (rest tl)] 
                 ;; recurse 
                 (list op (infix-to-prefix hd) (infix-to-prefix tl)))))))) 
; I renamed Jeff's macro 'formula' to just '|' for brevity.
(defmacro |
  "Formula macro translates from infix to prefix" 
  ([& equation] 
     (infix-to-prefix equation)))