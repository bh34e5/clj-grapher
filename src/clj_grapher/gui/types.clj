(ns clj-grapher.gui.types)

(defrecord Application [function
                        show-mod-lines
                        show-arg-lines
                        scale
                        event-system])

(defn set-function! [application func]
  (dosync
    (alter application assoc :function func)))

(defn set-show-lines! [application line-type show?]
  (dosync
    (alter application assoc line-type show?)))

(defn set-scale! [application scale]
  (dosync
    (alter application assoc :scale scale)))

(defn register-event-listener!
  [application event-name listener]
  (dosync
    (let [id (gensym)
          modify (fn [application]
                   (let [e-sys (:event-system application)
                         found-or-map (get e-sys event-name {})
                         upd-listener (assoc found-or-map id listener)
                         upd (assoc e-sys event-name upd-listener)]
                     (assoc application :event-system upd)))]
      (alter application modify)
      id)))

(defn deregister-event-listener!
  [application event-name id]
  (dosync
    (let [modify (fn [application]
                   (let [e-sys (:event-system application)
                         cur (get e-sys event-name)
                         cur-or-empty (or cur {})
                         upd (dissoc cur-or-empty id)]
                     (if cur
                       (assoc application
                            :event-system
                            (assoc e-sys event-name upd))
                       application)))]
      (alter application modify))))

(defn deregister-all-event-listeners!
  [application & event-names]
  (dosync
    (let [modify (fn [application]
                   (let [e-sys (:event-system application)
                         after-removal (apply dissoc e-sys event-names)]
                     (assoc application :event-system after-removal)))]
      (alter application modify))))

(defn notify [application event-name & args]
  (println "Got a notification!" event-name args)
  (doseq [listener (vals (get (:event-system @application) event-name))]
    (apply listener args)))
