(ns grid)

(def origin [0 0])

(def north [-1 0])
(def east [0 1])
(def south [1 0])
(def west [0 -1])

(def turn-left
  {north west
   east north
   south east
   west south})

(def turn-right
  {north east
   east south
   south west
   west north})

(defn manhattan-distance
  [[y1 x1] [y2 x2]]
  (+ (abs (- y1 y2)) (abs (- x1 x2))))

(defn move
  ([location direction]
   (mapv + location direction))
  ([location direction distance]
   (mapv + location (mapv (partial * distance) direction))))
