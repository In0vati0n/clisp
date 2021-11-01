;; str lib

(def! str-first-char
  (fn* [string]
    (first (seq string))
  )
)

(def! str-rest-str
  (fn* [string]
    (apply str (rest (seq string)))
  )
)

(def! str-sub
  (fn* [string start-idx end-idx]
    (let* [str-sub-inner (fn* [string idx]
                           (cond
                             (< idx start-idx)
                             (str-sub-inner (str-rest-str string)
                                            (+ 1 idx)
                             )

                             (>= idx end-idx)
                             ""

                             "else"
                             (str (str-first-char string)
                                  (str-sub-inner (str-rest-str string)
                                                 (+ 1 idx)
                                  )
                             )
                           )
                         )
          ]
      (str-sub-inner string 0)
    )
  )
)

