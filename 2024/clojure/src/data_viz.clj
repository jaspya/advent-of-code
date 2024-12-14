(ns data-viz
  (:import
   [java.awt
    BorderLayout
    Color
    Graphics2D
    GridLayout]
   [javax.swing
    JFrame
    JPanel]))

(defonce panel-container (JPanel.))

(defn- create-grid-panel
  [grid-rows grid-cols points]
  (proxy [JPanel] []
    (paintComponent [^Graphics2D g]
      (let [width (.getWidth this)
            height (.getHeight this)
            size (min width height)
            cell-size (min (/ width grid-cols)
                           (/ height grid-rows))]

        (.setColor g (Color. 16 16 16))
        (.fillRect g 0 0 width height)
        (.setColor g Color/BLACK)
        (.fillRect g 0 0 size size)

        (.setColor g Color/WHITE)
        (doseq [[x y] points]
          (.fillOval g (* x cell-size) (* y cell-size)
                     (max 2 (quot cell-size 2))
                     (max 2 (quot cell-size 2))))))))

(defn- draw-panels
  [data-fn panel-config]
  (.removeAll panel-container)
  (let [{:keys [panel-count grid-rows grid-cols start-index step-count]} panel-config]
    (doseq [idx (range panel-count)]
      (let [data (data-fn (+ start-index (* idx step-count)))]
        (.add panel-container (create-grid-panel grid-rows grid-cols data))))
    (.revalidate panel-container)
    (.repaint panel-container)))

(defn start
  []
  (doto (JFrame. "Data Viz: Where is my data?")
    (.setLayout (GridLayout. 1 1))
    (.add panel-container BorderLayout/CENTER)
    (.setSize 800 600)
    (.setVisible true)))

(defn draw
  [data-fn {:keys [panel-cols] :as panel-config}]
  (.setBackground panel-container (Color. 16 16 16))
  (.setLayout panel-container (GridLayout. 0 panel-cols 10 10))
  (draw-panels data-fn panel-config))
