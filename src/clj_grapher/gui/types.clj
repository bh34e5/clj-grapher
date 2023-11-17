(ns clj-grapher.gui.types)

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
  (set-function! [application func])
  (set-show-lines! [application line-type is-show])
  (set-scale! [application scale]))

(deftype Application [function
                      show-mod-lines
                      show-arg-lines
                      scale
                      event-system]
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [_ k not-found]
    (case k
      :function function
      :show-mod-lines show-mod-lines
      :show-arg-lines show-arg-lines
      :scale scale
      :event-system event-system
      not-found))

  IApplication
  (set-function! [_ func]
    (Application. func show-mod-lines show-arg-lines scale event-system))
  (set-show-lines! [_ line-type is-show]
    (let [cur {:show-mod-lines show-mod-lines
               :show-arg-lines show-arg-lines}
          upd (assoc cur line-type is-show)]
      (Application. function
                    (:show-mod-lines upd)
                    (:show-arg-lines upd)
                    scale
                    event-system)))
  (set-scale! [_ upd-scale]
    (Application. function
                  show-mod-lines
                  show-arg-lines
                  upd-scale
                  event-system))

  IEventSystem
  ;;; TODO: figure out how I want to do this, because right now,
  ;;;       calling add will add to the application system, but then
  ;;;       return the id so that it can be removed. But I suppose
  ;;;       rather than holding on to an id, I can just hold on to
  ;;;       the function itself and use that to remove...
  (add-event-listener! [_ event-name listener]
    (let [name-set (get event-system event-name #{})
          upd (conj name-set listener)]
      (Application. function
                    show-mod-lines
                    show-arg-lines
                    scale
                    (assoc event-system event-name upd))))
  (rem-event-listener! [_ event-name listener]
    (let [name-set (get event-system event-name #{})
          upd (remove #{listener} name-set)]
      (Application. function
                    show-mod-lines
                    show-arg-lines
                    scale
                    (assoc event-system event-name upd))))
  (drop-events [_ event-names]
    (let [upd (apply dissoc event-system event-names)]
      (Application. function show-mod-lines show-arg-lines scale upd)))
  (notify* [_ event-name args]
    (let [name-set (get event-system event-name #{})]
      (doseq [listener name-set]
        ;;; TODO: remove the deref, right now it's here to realize exceptions
        ;;;       that are getting thrown in the pool thread
        @(future (apply listener args)))))

  Object
  (toString [_]
    (str "Application{"
         "function: " function
         ", show-mod-lines: " show-mod-lines
         ", show-arg-lines: " show-arg-lines
         ", scale: " scale
         ", event-system: " event-system
         "}")))

(defn map->Application [arg-map]
  (Application. (:function arg-map)
                (:show-mod-lines arg-map)
                (:show-arg-lines arg-map)
                (:scale arg-map)
                (:event-system arg-map)))
