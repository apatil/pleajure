; Rules:
;  comma: next line continues this line.
;  backslash: Close the paren on this line, then next line continues it.

(def space-tab "    ")

(defn num-tabs [line]
    "Number of tabs at beginning of line" 
    (if (.startsWith line space-tab)
        (+ 1 (num-tabs (.substring line 4)))
        0))
(defn add-tabs-before [line n]
    (if (= n 0)
        line
        (.concat space-tab (add-tabs-before line (- n 1)))))
(defn comma? [line] "Whether line ends with comma" (.endsWith (.trim line) ","))
(defn comment? [line] "Whether line ends with comment" (.startsWith (.trim line) ";"))
(defn file-lines [fname] (.split (slurp fname) "\n"))

(defn join-line 
    "Joins lines with newlines."
    ([] nil)
    ([line1] line1)
    ([line1 line2] (.concat (.concat line1 "\n") line2)))
(defn join-lines [lines] (reduce join-line lines))

(defn add-opening-paren [line] "Adds an opening paren to line." (.concat "(" line))
(defn add-closing-parens [line n] "Adds n closing parens to lines." 
    (if (= n 0) 
        line 
        (.concat (add-closing-parens line (- n 1)) ")")))
(defn clean [line] "Trims whitespace from a line. Mappable." (.trim line))

(defn update-lines [last-line this-line last-tab this-tab skip-paren] 
    (let [tab-diff (- last-tab this-tab)
        closing-parens (if (.startsWith last-line "(") 1 0)]
    (if skip-paren 
        (list last-line this-line) 
        (list (add-closing-parens last-line (max (+ tab-diff closing-parens) 0) ) (add-opening-paren this-line)))))

(defn start-string? [line] (.startsWith line "\""))
(defn still-in-string? [line in-string] (if in-string (not (.endsWith line "\"")) (.startsWith line "\"")))
(defn comment-or-quote? [line] (or (comment? line) (.startsWith line "`") (.startsWith line "'")))

(defn plj-to-clj 
    "Converts a clj file to lines that form a clj file."
    ([fname] 
        (let [lines (file-lines fname)
                tabs (map num-tabs lines)
                lines (map clean lines)
                first-line (first lines)] 
            (plj-to-clj 
                (if (comment-or-quote? first-line) first-line (add-opening-paren first-line))
                (second lines) 
                (rrest lines) 
                (first tabs) 
                (second tabs)
                (rrest tabs) 
                (comma? (first lines))
                []
                []
                false
                false)))
    ([last-line this-line rest-lines last-tab this-tab rest-tabs last-comma out-lines noncode-lines in-string last-noncode]
        
        (if this-line
            ; If there are still lines:
            (let [in-string (if in-string in-string (start-string? this-line))
                    next-in-string (if (comment? this-line) in-string (still-in-string? this-line in-string))
                    comment-or-quote (comment-or-quote? this-line)]
                (if (or comment-or-quote in-string (= (count this-line) 0))

                    ; Treat non-code lines (comments and docstrings) specially.
                    (plj-to-clj
                        last-line (first rest-lines) (rest rest-lines)
                        last-tab (first rest-tabs) (rest rest-tabs)
                        last-comma out-lines (conj noncode-lines (add-tabs-before this-line this-tab)) next-in-string true)

                    ; Add parens to code lines as needed.
                    (let[this-comma (comma? this-line)
                        new-lines (update-lines last-line this-line last-tab this-tab last-comma)
                        last-line (add-tabs-before (first new-lines) last-tab)
                        this-line (second new-lines)]                    

                        (plj-to-clj
                            this-line (first rest-lines) (rest rest-lines)
                            this-tab (first rest-tabs) (rest rest-tabs)
                            this-comma (vec (concat (conj out-lines last-line) noncode-lines)) [] next-in-string false))))

            (let[new-lines (update-lines last-line "" last-tab 0 last-noncode)
                last-line (add-tabs-before (first new-lines) last-tab)]
                (vec (concat (conj out-lines last-line) noncode-lines))))))

(defn with-pleajure [fname] (load-string (join-lines (plj-to-clj fname))))

;(map num-tabs (file-lines "test.plj"))
;(map comma? (file-lines "test.plj"))
;(map slash? (file-lines "test.plj"))