;; i32 add(i32 x, i32 y) {
;;     return x + y;
;; }
(def! add
      (fn* [x :i32 y :i32] :i32
           (do
                (return (+ x y))
           )
      )
)

;; void main() {
;; }
(def! main
      (fn* []
           (do
                (println "hello world")
           )
      )
)

