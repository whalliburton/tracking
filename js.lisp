(in-package :tracking-js)

(defpsmacro with-id ((var id) &body body)
  `(let ((,var (get-by-id ,id)))
     (if ,var
       (progn
         ,@body))))

(defpsmacro plusp (el)
  `(> ,el 0))

(defpsmacro console (&rest rest)
  `(cond
     ((=== *browser* :firefox) ((@ console log) ,@rest))
     ((or (=== *browser* :chrome) (=== *browser* :trident))
      ((@ console log)
       (+ ,@(butlast (loop for arg in rest
                           nconc (list arg " "))))))))

(defpsmacro create-element (node-type)
  `((@ document create-element) ,node-type))

(defpsmacro set-inner-html (el html)
  `(setf (slot-value ,el 'inner-h-t-m-l) ,html))

(defpsmacro remove-node (el)
  `((@ ,el parent-node remove-child) ,el))

(defparameter *pixel-styles*
  '(top bottom left right width height border-width))

;; FIXME might be faster to setAttributes
(defpsmacro set-style ((&rest var) &rest args)
  `(setf
    ,@(loop for (a b) on args by #'cddr
            nconc
            `((@ ,@var style ,a)
                       ,(if (and b (member a *pixel-styles*)) `(+ ,b "px") b)))))

(defun ensure-string (symbol-or-string)
  (etypecase symbol-or-string
    (symbol (symbol-name symbol-or-string))
    (string symbol-or-string)))

(defun string-starts-with (string prefix &key (test #'char=))
  "Returns true if STRING starts with PREFIX."
  (let ((prefix (ensure-string prefix))
        (string (ensure-string string)))
    (let ((mismatch (mismatch prefix string :test test)))
      (or (not mismatch) (= mismatch (length prefix))))))

(defun this-swap (from to)
  (cond
    ((eql from 'this) to)
    (t
     (let ((sfrom (symbol-name from))
           (sto (symbol-name to)))
       (and (string-starts-with sfrom "THIS.")
            (intern (concatenate 'string sto "." (subseq sfrom 5))))))))

(defun subthis (this tree)
  (labels ((s (subtree)
             (or (and (symbolp subtree) (this-swap subtree this))
                 (cond ((atom subtree) subtree)
                       (t (let ((car (s (car subtree)))
                                (cdr (s (cdr subtree))))
                            (if (and (eq car (car subtree))
                                     (eq cdr (cdr subtree)))
                              subtree
                              (cons car cdr))))))))
    (s tree)))

(defpsmacro defun-trace (name args &rest body)
  (let* ((sname (ps::symbol-to-js-string name))
         (tname (ps-gensym name))
         (this (ps-gensym "this"))
         (arg-names (loop for arg in args
                          unless (eq arg '&optional)
                            collect (if (consp arg) (car arg) arg)))
         (argpairs
          (loop for arg in arg-names
                nconc (list (ps::symbol-to-js-string arg) arg))))
    `(progn
       (defun ,tname (,this ,@args)
         ,@(subthis this body))
       (defun ,name ,arg-names
         (console *trace-level* ,sname ":" ,@argpairs)
         (incf *trace-level*)
         (let ((rtn (,tname this ,@arg-names)))
           (decf *trace-level*)
           (console *trace-level* ,sname "returned" rtn)
           (return rtn))))))

(defparameter *js-file*
  (ps*
   '(progn

     (defvar *trace-level* 0)

     (defvar *browser*
       (cond
         ((plusp ((@ (@ navigator user-agent) index-of) "Chrome")) :chrome)
         ((plusp ((@ (@ navigator user-agent) index-of) "Gecko")) :firefox)
         ((plusp ((@ (@ navigator user-agent) index-of) "MSIE")) :trident)
         (t :unknown)))

     (defun show (id)
       (with-id (o id)
         (setf (@ o style visibility) "visible")))

     (defun hide (id)
       (with-id (o id)
         (setf (@ o style visibility) "hidden")))

     (defun visit (id)
       (setf (@ window location) (+ "/?id=" id)))

     (defun word (id index)
       (setf (@ window location) (+ "/?id=" id "&index=" index)))

     (defun word-text (id index)
       (setf (@ window location) (+ "/?id=" id "&word=" index)))

     (defun go (url) (setf (@ window location) url))

     (defun get-by-id (id &optional (error t))
       (let ((hit ((@ document get-element-by-id) id)))
         (if hit
           (return hit)
           (if error (console "ERROR: get-by-id" id)))))

     (defvar *editor*)

     (defun add-new-child (element position name &optional attributes body)
       (let ((new-element (create-element name))
             (num (if attributes (/ (@ attributes length) 2) 0)))
         (dotimes (x num)
           (let ((name (aref attributes (* 2 x)))
                 (value (aref attributes (1+ (* 2 x)))))
             (set-att new-element name value)))
         (when body (set-inner-html new-element body))
         (cond
           ((and (= position "beginning") (@ element first-child))
            ((@ element insert-before) new-element (@ element first-child)))
           (t ((@ element append-child) new-element)))
         (return new-element)))

     (defun is-trident ()
       "Return the Trident version; NIL if the client is not Internet Explorer."
       (let ((foundp ((@ (@ navigator user-agent) search)
                      "MSIE ([0-9]{1,}[\.0-9]{0,})")))
         (when (> foundp -1)
           (return (parse-float (@ *reg-exp '$1))))
         (return nil)))

     (defun set-att (el name value)
       (let ((o (if (= (typeof el) "string") (get-by-id el) el)))
         (if value
           ;; IE's setAttribute doesn't work with `style' and `class'
           ;; In 8.0 they corrected the latter (obliterating the previous approach)
           ;; but kept the former.
           ;; TODO: there are some other attributes, see http://gist.github.com/266694
           (cond
             ((and (is-trident)
                   (= name "style"))
              (setf (@ (@ o style) 'css-text) value))
             ((and (is-trident) (< (is-trident) 8)
                   (= name "class"))
              ((@ o set-attribute) "className" value))
             (t
              ((@ o set-attribute) name value)))
           ((@ o remove-attribute) name))))

     (defun get-offset (el)
       (let ((left (parse-int (@ el offset-left)))
             (top (parse-int (@ el offset-top)))
             (scroll-left (@ el scroll-left))
             (scroll-top (@ el scroll-top)))
         ;; http://doiop.com/ie+offsetparent
         (try (setf el (@ el offset-parent)) (:catch (e)))
         (while (!== el null)
           (incf left (parse-int (@ el offset-left)))
           (incf top (parse-int (@ el offset-top)))
           (incf scroll-left (@ el scroll-left))
           (incf scroll-top (@ el scroll-top))
           (try (setf el (@ el offset-parent)) (:catch (e))))
         (return (create :left left :top top
                         :scrollleft scroll-left :scrolltop scroll-top))))

     (defun show-editor (event target-id left width height contents)
       (let* ((target (get-by-id target-id))
              (target-position (get-offset target)))
         (if *editor*
           (setf (@ *editor* style top) (@ target-position top))
           (setf *editor*
                 (add-new-child (get-by-id "body")
                                "beginning" "div"
                                (array :class "editor"
                                       :style (+ "position:absolute;"
                                                 "top:" (@ target-position top) ";"
                                                 "left:" left ";"
                                                 "float:right;")))))
         (set-inner-html *editor*
                         (+ "<table><tr><td>"
                            (+ "<textarea id=\"edta\" style=\"background-color:black;color:white;"
                               "padding:10px;border-radius:5px;"
                               "width:" width "px;height:" height "px;\" >"
                               contents
                               "</textarea>")
                            "</td></tr><tr><td>"
                            "<img id=\"edtab\" class=\"page-button\" src=\"/images/check.png\" />"
                            "</td></tr></table></div>"))
         (let ((textarea (get-by-id "edta"))
               (check (get-by-id "edtab")))
           (setf (@ check onclick)
                 (lambda (event)
                   (add-comment target (@ textarea value))
                   ((@ *editor* remove))
                   (setf *editor* nil)))
           (setf (@ textarea onchange)
                 (lambda (event)
                   (let ((text (@ textarea value)))
                     (add-comment target text t)
                     (setf (@ target draft) text)))))
         (when (@ target draft)
           (setf (@ textarea value) (@ target draft)))
         ((@ textarea focus))))

     (defun request (command &optional arguments)
       (unless arguments (setf arguments (create)))
       (setf (@ arguments :command) command)
       (http-request "/a" arguments))

     (defun add-comment (target text &optional draft)
       (request (if draft "add-comment-draft" "add-comment")
                (create :text text :target (@ target id))))

     (defun encode-arguments (arguments)
       (let ((arr (make-array)))
         (for-in (key arguments)
           (let ((val (aref arguments key)))
             (setf
              (aref arr (length arr))
              (+ (encode-u-r-i-component key) "=" (encode-u-r-i-component val)))))
         (return ((@ ((@ arr join) "&") replace) (regex "/%20/g") "+"))))

     (defvar *print-response* nil)

     (defun http-request (url arguments)
       (let* ((encoded-arguments (when arguments (encode-arguments arguments)))
              (req (new |:XMLHttpRequest|))
              (change-fn
                (lambda ()
                  (when (!= (@ req ready-state) 4) (return))
                  (unless (= (slot-value req 'status) 200)
                    (console "ERROR: ajax" req)
                    (return))
                  (set-timeout
                   (lambda ()
                     (let ((response (slot-value req 'response-text)))
                       (when *print-response* (console "response:" response))
                       (eval response)))
                   10))))
         ((@ req open) "GET" (if encoded-arguments (+ url "&" encoded-arguments) url) t)
         ((@ req set-request-header) "X-Requested-With" "XMLHttpRequest")
         (setf (@ req onreadystatechange) change-fn)
         ((@ req send) nil)))

     (defun show-settings ()
       (show "settings-icon"))

     (defun hide-settings ()
       (hide "settings-icon"))

     (defun show-settings-dialog ()
       (close-dialog t)
       (request "show-settings-dialog"))

     (defun show-avatars-dialog ()
       (close-dialog t)
       (request "show-avatars-dialog"))

     (defvar *backpane*)
     (defvar *dialog*)

     (defun show-dialog (html)
       (let ((body (get-by-id "body")))
         (unless *backpane*
           (setf *backpane*
                 (add-new-child body nil "div"
                                (array :class "dialog-backpane"))))
         (setf *dialog*
               (add-new-child body nil "div"
                              (array :class "dialog-outer")
                              (+ "<div class=\"dialog-inner\"><div class=\"dialog\">"
                                 html "</div></div>")))))

     (defun close-dialog (&optional nobackpane)
       (when *backpane*
         (unless nobackpane
           (remove-node *backpane*)
           (setf *backpane* nil))
         (remove-node *dialog*)
         (setf *dialog* nil)))

     (defvar *marker* nil)
     (defvar *map* nil)

     (defun latlng (lat lng)
       (return (new ((@ google maps *lat-lng ) lat lng))))

     (defun latitude (latlng) (return ((@ latlng lat))))
     (defun longitude (latlng) (return ((@ latlng lng))))

     (defun icon-src (icon)
       (return
         (+ "/images/" icon
            (if (= ((@ icon index-of) "v/") 0)
              ""
              ".png"))))

     (defun make-marker (map position title &optional icon)
       (let ((marker
               (new ((@ google maps *marker)
                     (create :position position
                             :map map
                             :title title
                             :icon (and icon (icon-src icon)))))))
         (return marker)))

     (defun remove-marker (marker)
       ((@ marker set-map) nil))

     (defvar *roadmap* (@ google maps *map-type-id *r-o-a-d-m-a-p))

     (defun make-map (id center zoom)
       (let ((ids (array)))
         (for-in (type (@ google maps *map-type-id))
           ((@ ids push) (slot-value (@ google maps *map-type-id) type)))
         ((@ ids push) "OSM")
         ((@ ids push) "toner")
         ((@ ids push) "terrain")
         ((@ ids push) "watercolor")
         (let ((map
                 (new ((@ google maps *map)
                       (get-by-id id)
                       (create :center center
                               :zoom zoom
                               :map-type-id *roadmap*
                               :street-view-control f
                               :map-type-control t
                               :map-type-control-options (create :map-type-ids ids)
                               :scale-control t)))))
           ((@ map map-types set)
            "OSM" (new ((@ google maps *image-map-type)
                        (create
                         :get-tile-url (lambda (coord zoom)
                                         (return (+ "http://tile.openstreetmap.org/"
                                                    zoom "/" (@ coord x) "/" (@ coord y) ".png")))
                         :tile-size (new ((@ google maps *size) 256 256))
                         :name "OSM"
                         :max-zoom 18))))
           ((@ map map-types set)
            "toner" (new ((@ google maps *stamen-map-type) "toner")))
           ((@ map map-types set)
            "watercolor" (new ((@ google maps *stamen-map-type) "watercolor")))
           ;; FIXME why not work?
           ((@ map map-types set)
            "terrain" (new ((@ google maps *stamen-map-type) "terrain")))

           (return map))))

     (defun add-listener (type fn &optional (what *map*))
       ((@ google maps event add-listener) what type fn))

     (defun mapto (id lat lng zoom name onload controls-id)
       (let* ((latlng (latlng lat lng))
              (map (make-map id latlng zoom)))
         (setf *map* map)
         (add-listener "dragend" (lambda () (send-dragend)))
         (add-listener "zoom_changed"
                       (lambda ()
                         (let ((center ((@ *map* get-center))))
                           (request "handle-map-zoom-changed"
                                    (create :bounds (map-bounds) :zoom ((@ *map* get-zoom)))))))
         (add-listener "click"
                       (lambda (event)
                         (request "handle-map-click" (create :lat (latitude (@ event lat-lng))
                                                             :lng (longitude (@ event lat-lng))))))
         (when onload
           (add-listener "tilesloaded" (lambda (event) (eval onload))))
         (when controls-id
           (add-listener "center_changed" (lambda () (update-controls controls-id))))))

     (defun update-controls (id)
       (let ((el (get-by-id id nil)))
         (when el
           (let* ((center ((@ *map* get-center)))
                  (lat ((@ (latitude center) to-fixed) 4))
                  (north (> lat 0))
                  (lng ((@ (longitude center) to-fixed) 4))
                  (east (> lng 0)))
             (set-inner-html el (+ ((@ *math abs) lng) (if east "E" "W")
                                   " " ((@ *math abs) lat) (if north "N" "S")
                                   " " ((@ *map* get-zoom))))))))

     (defun send-dragend ()
       (let ((center ((@ *map* get-center))))
         (request "handle-map-dragend"
                  (create :bounds (map-bounds)))))

     (defun move-map (lat lng &optional zoom)
       ((@ *map* set-center) (latlng lat lng))
       (when zoom ((@ *map* set-zoom) zoom)))

     (defun map-bounds ()
       (let* ((bounds ((@ *map* get-bounds)))
              (sw ((@ bounds get-south-west)))
              (ne ((@ bounds get-north-east))))
         (return (list (latitude sw) (longitude sw) (latitude ne) (longitude ne)))))

     (defun move-marker (lat lng name)
       (if *marker*
         (progn
           ((@ *marker* set-position) (latlng lat lng))
           ((@ *marker* set-title) name))
         (setf *marker* (make-marker *map* (latlng lat lng) name))))

     (defun center-on-marker (&optional zoom)
       ((@ *map* set-center) ((@ *marker* get-position)))
       (when zoom ((@ *map* set-zoom) zoom)))

     (defun center-map (lat lng)
       ((@ *map* set-center) (latlng lat lng)))

     (defun send-new-map-location (el)
       (request "set-map-location" (create :name (@ el value)
                                           :bounds (map-bounds)))
       (setf (@ el value) ""))

     (defun send-client-location ()
       (when (@ navigator geolocation)
         ((@ navigator geolocation get-current-position)
          (lambda (pos)
            (request "set-client-location"
                     (create :lat (@ pos coords latitude)
                             :lng (@ pos coords longitude)))))))

     (defun set-contents (id body)
       (set-inner-html (get-by-id id) body))

     (defun select-maplist (name lat lng)
       (request "select-maplist"
                (create :name name :lat lat :lng lng)))

     (defvar *pois* nil)

     (defun setup-pois (pois)
       (let ((pois (eval pois)))
         (when *pois* (loop for (box-id poi) in *pois* do (remove-marker poi)))
         (setf *pois*
               (loop for (name lat lng icon box-id) in pois
                     collect
                        (list box-id
                              (let ((marker (make-marker *map* (latlng lat lng) name icon)))
                                (when box-id (setup-box-listeners marker box-id))
                                marker))))))

     (defun find-poi (id)
       (loop for (box-id poi) in *pois*
             when (= box-id id)
             do (return poi))
       (error "unknown poi" id))

     (defun hilight-poi (id)
       (hilight-marker (find-poi id)))

     (defun unhilight-poi (id)
       (unhilight-marker (find-poi id)))

     (defun hilight-marker (marker)
       (let ((icon ((@ marker get-icon))))
         (setf (slot-value marker 'old-icon) icon)
         ((@ marker set-icon) (+ icon "?hilight=yes"))))

     (defun unhilight-marker (marker)
       (let ((icon ((@ marker get-icon))))
         ((@ marker set-icon) (slot-value marker 'old-icon))))

     (defun setup-box-listeners (marker id)
       (add-listener "mouseover" (lambda () (hilight-box id)) marker)
       (add-listener "mouseout" (lambda () (unhilight-box id)) marker))

     (defun hilight-box (id)
       (let ((el (get-by-id id)))
         (setf (slot-value el 'saved-background) (@ el style background-color))
         (set-style (el) background-color "#444")))

     (defun unhilight-box (id)
       (let ((el (get-by-id id)))
         (set-style (el) background-color (slot-value el 'saved-background))))

     (defvar *info-window* nil)

     (defun info-window (lat lng content)
       (when *info-window*
         ((@ *info-window* close)))
       (setf *info-window*
             (let ((window (new ((@ google maps *info-window)))))
               ((@ window set-content) content)
               ((@ window set-position) (latlng lat lng))
               ((@ window open) *map*)
               window)))

     (defun start-iscroll (id)
       (new (i-scroll id (create :h-scrollbar nil :v-scrollbar nil))))

     (defun set-class (id new-class)
       (let ((el (get-by-id id)))
         (setf (slot-value el 'class-name) new-class)))

     (defun set-map-styles (styles)
       ((@ *map* set-options) (create :styles (eval styles))))

     (defun timestamp ()
       (return (new ((@ (*date ) get-time)))))

     (defun reload-calibration-image ()
       (let ((el (get-by-id "calibration")))
         (setf (slot-value el 'src) (+ "/images/calibration.png?id=" (timestamp)))))

     (defun js-ensure-list (x) ; prevent name conflict with metatilities ensure-list
       (if (= (type-of x) "array")
         (return x)
         (return (array x))))

     (defun type-of (o)
       "Sane typeof operator."
       ;; http://javascript.crockford.com/remedial.html
       (let ((type (typeof o)))
         (cond
           ((and (=== type "object") o (instanceof o *array))
            (return "array"))
           ((=== type "object")
            (return nil))
           (t
            (return type)))))

     (defun set-unselectable (ids)
      (dolist (id (js-ensure-list ids))
        (let ((o (get-by-id id)))
          (when o
            (console o)
            (setf (@ o unselectable) "on"
                  (@ o onselectstart) (lambda () (return false)))
            (when (@ o style)
              (setf (@ o style *moz-user-select) "none")))))))))

(defun js-file () *js-file*)


