;; maps
{:a '(hi hello) :b 10}

;; lambdas
#(+ % %2)

;; 
(defn f [x]
  (let [y 10
        z (+ x y)]
    (map #(+ % 1) (list x y z))))

;; lets
(let [x 10] (f x))
