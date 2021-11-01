(load-file "./cl/str.cl")
(load-file "./cl/list.cl")

;; get type from symbol
;; :i32 -> i32
(def! get-type-symbol
  (fn* [symbol]
    (let* [symbol (str symbol)
           first-char (str-first-char symbol)
          ]
      (cond
        (= ":" first-char)
        (str-rest-str symbol)

        "else"
        symbol
      )
    )
  )
)

(def! compile-arguments
  (fn* [args]
    (do
      (list-join-with-custom-str args ", " pr-str)
    )
  )
)

(def! compile-params
  (fn* [params]
    (do
      ""
    )
  )
)

(def! compile-statements
  (fn* [statements]
    (cond
      (nil? statements)
      ""

      (symbol? statements)
      (str statements)

      (list? statements)
      (let* [symbol (first statements)
             rest-statements (rest statements)
            ]
        (do
          (cond
            (= 'do symbol)
            (str
              "{\n"
              (compile-statements (first rest-statements))
              "\n}"
            )
  
            (= 'return symbol)
            (str
              "return "
              (compile-statements (first rest-statements))
              ";"
            )
  
            (= '+ symbol)
            (str
              (compile-statements (first rest-statements))
              " + "
              (compile-statements (nth rest-statements 1))
            )

            "else"
            (str
              symbol "("
              (compile-arguments rest-statements)
              ")"
            )
          )
        )
      )
    )
  )
)

(def! compile-def-func
  (fn* [def-func-list-value]
    (do
      (cond
        (nil? def-func-list-value)
        nil

        "else"
        (let* [func-name (nth def-func-list-value 1)
               func-body (nth def-func-list-value 2)
               func-params (nth func-body 1)
               func-return-type (if (= 4 (count func-body))
                                  (nth func-body 2)
                                  "void"
                                )
               func-body (if (= 4 (count func-body))
                           (nth func-body 3)
                           (nth func-body 2)
                         )
              ]
          (do
            (println (str "func-name:        " func-name))
            (println (str "func-params:      " func-params))
            (println (str "func-return-type: " func-return-type))
            (println (str "func-body:        " func-body))
            (println "----------")

            (let* [func-code
                             (str
                               (get-type-symbol func-return-type) " "
                               func-name " ("
                               (compile-params func-params)
                               ") "
                               (compile-statements func-body)
                             )
                  ]
              (do
                (println func-code)
                func-code
              )
            )
          )
        )
      )
    )
  )
)


(def! compile
  (fn* [path]
    (do
      (println (str "compile file: " path))
      (let* [root
                  (read-string
                    (str
                      "(do "
                      (slurp path)
                      "\nnil)"
                    )
                  )
            ]
         (map compile-def-func (rest root))
      )
    )
  )
)

(if (empty? *ARGV*)
  (println "input file not found!")
  (compile (first *ARGV*))
)