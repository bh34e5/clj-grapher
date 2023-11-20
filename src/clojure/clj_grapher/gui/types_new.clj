(ns clj-grapher.gui.types-new)

(defprotocol IEventSystem
  (add-event-listener! [system event-name listener])
  (rem-event-listener! [system event-name listener])
  (drop-events [system event-names])
  (notify* [system event-name args]))

;; Helper function to allow calling notify without wrapping arguments
;; at the call site
(defn notify [system event-name & args]
  (notify* system event-name args))

(defprotocol IApplication
  (as-map [application])
  (set-field [application field-or-path upd-value]))

(declare map->Application)
(deftype Application [function
                      flags
                      config
                      event-system]
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [_ k not-found]
    (case k
      :function function
      :flags flags
      :config config
      :event-system event-system
      not-found))

  IApplication
  (as-map [_]
    {:function function
     :flags flags
     :config config
     :event-system event-system})
  (set-field [this field-or-path upd-value]
    (if-not (coll? field-or-path)
      (let [upd-values (assoc (as-map this) field-or-path upd-value)]
        (map->Application upd-values))
      (throw (UnsupportedOperationException. "Not implemented yet!"))))

  IEventSystem
  ;;; TODO: figure out how I want to do this, because right now,
  ;;;       calling add will add to the application system, but then
  ;;;       return the id so that it can be removed. But I suppose
  ;;;       rather than holding on to an id, I can just hold on to
  ;;;       the function itself and use that to remove...
  ;;; TODO: consider turning these into `weak-ref`s and occasionally
  ;;;       (on the addition of a new listener?) check for nil listeners
  ;;;       and remove those that have expired
  (add-event-listener! [this event-name listener]
    (let [name-set (get event-system event-name #{})
          upd (conj name-set listener)]
      (map->Application (assoc (as-map this)
                               :event-system
                               (assoc event-system
                                      event-name
                                      upd)))))
  (rem-event-listener! [this event-name listener]
    (let [name-set (get event-system event-name #{})
          upd (remove #{listener} name-set)]
      (map->Application (assoc (as-map this)
                               :event-system
                               (assoc event-system
                                      event-name
                                      upd)))))
  (drop-events [this event-names]
    (let [upd (apply dissoc event-system event-names)]
      (map->Application (assoc (as-map this)
                               :event-system
                               upd))))
  (notify* [_ event-name args]
    (let [name-set (get event-system event-name #{})]
      (doseq [listener name-set]
        (apply listener args))))

  Object
  (toString [this]
    (str (as-map this))))

(defn map->Application [arg-map]
  (Application. (:function arg-map)
                (:flags arg-map)
                (:config arg-map)
                (:event-system arg-map)))
