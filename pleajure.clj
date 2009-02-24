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
(defn file-lines [fname] (.split (slurp fname) "\n"))

(defn join-line  [sym]
    "Joins lines with newlines."
    (fn
        ([] nil)
        ([line1] line1)
        ([line1 line2] (.concat (.concat line1 sym) line2))))
(defn join-lines [lines sym] (reduce (join-line sym) lines))

(defn add-opening-paren [line] "Adds an opening paren to line." (.concat "(" line))
(defn add-closing-parens [line n] "Adds n closing parens to lines." 
    (if (= n 0) 
        line 
        (.concat (add-closing-parens line (- n 1)) ")")))
(defn clean [line] "Trims whitespace from a line. Mappable." (.trim line))

(defn update-lines [last-line this-line last-tab this-tab] 
    (let [tab-diff (- last-tab this-tab)
        closing-parens (if (.startsWith last-line "(") 1 0)]
        (list (add-closing-parens last-line (max (+ tab-diff closing-parens) 0) ) (add-opening-paren this-line))))

(defn comma? [line] "Whether line ends with comma" (.endsWith line ","))
(defn start-string? [line] (.startsWith line "\""))
(defn still-in-string? [line in-string] (if in-string (not (.endsWith line "\"")) (.startsWith line "\"")))
(defn comment? [line] (.startsWith line ";"))

(defn translate-lines
    "Converts trimmed lines and tab levels of a plj file to lines that form a clj file."
    [lines tabs] 
    (let [first-line (first lines)] 

        (loop
            [last-line (if (comment-or-quote? first-line) first-line (add-opening-paren first-line)) 
            this-line (second lines) 
            rest-lines (rrest lines) 
            last-tab (first tabs) 
            this-tab (second tabs)
            rest-tabs (rrest tabs) 
            last-comma (comma? (first lines))
            out-lines []
            noncode-lines []
            in-string false
            last-noncode false
            tab-map {last-line last-tab}]
            (if this-line
                ; If there are still lines:
                (let [in-string (if in-string in-string (start-string? this-line))
                        next-in-string (if (comment? this-line) in-string (still-in-string? this-line in-string))]
                        
                    (if (or (comment? this-line) in-string (= (count this-line) 0))

                        ; Treat non-code lines (comments and docstrings) specially.
                        (recur
                            last-line 
                            (first rest-lines) 
                            (rest rest-lines)
                            last-tab 
                            (first rest-tabs) 
                            (rest rest-tabs)
                            last-comma 
                            out-lines 
                            (conj noncode-lines this-line) 
                            next-in-string 
                            true
                            (assoc tab-map this-line this-tab))

                        ; If the last line ended in a comma, just concatenate this line to it and continue.
                        (if last-comma
                            (recur
                                ((join-line "") last-line this-line)
                                (first rest-lines)
                                (rest rest-lines)
                                last-tab
                                (first rest-tabs)
                                (rest rest-tabs)
                                (comma? this-line)
                                out-lines
                                []
                                in-string
                                false
                                tab-map)
                            (let[this-comma (comma? this-line)
                            new-lines (update-lines last-line this-line last-tab this-tab)
                            last-line (first new-lines)
                            this-line (second new-lines)]                    

                            (recur
                                this-line 
                                (first rest-lines) 
                                (rest rest-lines)
                                this-tab 
                                (first rest-tabs) 
                                (rest rest-tabs)
                                this-comma 
                                (vec (concat (conj out-lines last-line) noncode-lines)) 
                                [] 
                                next-in-string 
                                false
                                (assoc tab-map last-line last-tab))))))

                (let[new-lines (update-lines last-line "" last-tab 0)
                    last-line (first new-lines)]
                    [(vec (concat (conj out-lines last-line) noncode-lines)) (assoc tab-map last-line last-tab)])))))

(defn plj-to-lines [fname]
    "Converts a plj file to the trimmed lines of a clj file, with a map of associated tab levels."
    (let [lines (file-lines fname)
        tabs (map num-tabs lines)
        lines (map clean lines)]
        (translate-lines lines tabs)))

(defn pleajure-print [fname] 
    "Converts a plj file to the text of a clj file and prints it."
    (let [lines-and-tabs (plj-to-lines fname)
        tabs (second lines-and-tabs)
        lines (first lines-and-tabs)
        add-tab (fn [line] (add-tabs-before line (tabs line)))]
        (print (join-lines (map add-tab lines) "\n"))))

(defn pleajure-load [fname] 
    "Converts a plj file to the text of a clj file and feeds it to the reader."
    (load-string (join-lines (first (plj-to-lines fname)) "\n")))