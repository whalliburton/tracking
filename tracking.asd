(defsystem :tracking
  :serial t
  :components ((:static-file "tracking.asd")
               (:file "package")
               (:file "rpc-sail")
               (:file "utility")
               (:file "icons")
               (:file "loader")
               (:file "build")
               (:file "serve")
               (:file "color")
               (:file "vector")
               (:file "calibration-image")
               (:file "render")
               (:file "debug")
               (:file "js")
               (:file "initialize"))
  :depends-on (:hunchentoot :deck-client :closure-html :alexandria :iterate :cl-who
               :parenscript :local-time :split-sequence :chronicity
               :drakma :cl-json :babel :anaphora :net-telent-date :fare-csv
               :vecto :cl-ppcre :lhstats))