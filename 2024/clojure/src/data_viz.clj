(ns data-viz
  (:import
   [java.awt
    BorderLayout
    Color
    Font
    Graphics2D
    GridLayout]
   [javax.swing JFrame JPanel]))

(defonce panel-container (JPanel.))

(defn- board->locations
  [board]
  (for [y (range (count board))
        x (range (count (first board)))]
    [y x]))

(def ^:private gcolors
  {:black (Color. 0 0 0)
   :blue (Color. 0 175 255)
   :green (Color. 0 215 0)
   :olive (Color. 135 175 0)
   :purple (Color. 175 135 255)
   :yellow (Color. 215 175 0)
   :red (Color. 255 0 0)
   :magenta (Color. 255 0 255)
   :orange (Color. 255 135 0)
   :white (Color. 255 255 255)
   :gray (Color. 158 158 158)})

(defn- create-grid-panel
  [grid-rows grid-cols points as-board colors as-points]
  (proxy [JPanel] []
    (paintComponent [^Graphics2D g]
      (let [width (.getWidth this)
            height (.getHeight this)
            size (min width height)
            cell-size (max 1 (min (quot width grid-cols)
                                  (quot height grid-rows)))]

        (.setColor g (Color. 0 0 0))
        (.fillRect g 0 0 width height)
        (.setColor g Color/BLACK)
        (.fillRect g 0 0 size size)

        (.setColor g Color/WHITE)
        (if as-board
          (do (.setFont g (Font. "Arial" Font/BOLD (quot cell-size 1)))
              (doseq [[y x] (board->locations points)]
                (let [tile (get-in points [y x])
                      wtf (get colors tile :white)]
                  (.setColor g (get gcolors wtf))
                  (if as-points
                    (.fillRect g (* x cell-size) (* y cell-size)
                               cell-size
                               cell-size)
                    (.drawString g tile
                                 (int (* x cell-size))
                                 (int (+ (* y cell-size) cell-size)))))))
          (doseq [[x y] points]
            (.fillRect g (* x cell-size) (* y cell-size)
                       cell-size
                       cell-size)))))))

(defn- draw-panels
  [data-fn panel-config]
  (.removeAll panel-container)
  (let [{:keys [panel-cols panel-rows panel-max grid-rows grid-cols start-index step-count as-board colors as-points]} panel-config
        panel-max (or panel-max (* panel-cols panel-rows))]
    (doseq [idx (range (min panel-max (* panel-cols panel-rows)))]
      (let [data (data-fn (+ start-index (* idx step-count)))]
        (.add panel-container (create-grid-panel grid-rows grid-cols data as-board colors as-points))))
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
  (.setBackground panel-container (Color. 0 0 0))
  (.setLayout panel-container (GridLayout. 0 panel-cols 10 10))
  (draw-panels data-fn panel-config))
