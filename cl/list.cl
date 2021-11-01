;; list lib

(def! list-join-with-custom-str
  (fn* [l sep str]
    (do
      (let* [inner-list-join (fn* [l sep]
                               (let* [f-elem (first l)
                                      r-elems (rest l)
                                     ]
                                 (cond
                                   (nil? f-elem)
                                   ""

                                   (empty? r-elems)
                                   (str f-elem)

                                   "else"
                                   (str f-elem sep (inner-list-join r-elems sep))
                                 )
                               )
                             )
            ]
        (inner-list-join l sep)
      )
    )
  )
)

(def! list-join
  (fn* [l sep]
    (list-join-with-custom-str l sep str)
  )
)