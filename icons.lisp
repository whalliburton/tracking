(in-package :tracking)

(eval-always
  (defparameter *icon-index*
   '((:glass                #\uf000)
     (:music                #\uf001)
     (:search               #\uf002)
     (:envelope             #\uf003)
     (:heart                #\uf004)
     (:star                 #\uf005)
     (:star-empty           #\uf006)
     (:user                 #\uf007)
     (:film                 #\uf008)
     (:th-large             #\uf009)
     (:th                   #\uf00a)
     (:th-list              #\uf00b)
     (:ok                   #\uf00c)
     (:remove               #\uf00d)
     (:zoom-in              #\uf00e)
     (:zoom-out             #\uf010)
     (:off                  #\uf011)
     (:signal               #\uf012)
     (:cog                  #\uf013)
     (:trash                #\uf014)
     (:home                 #\uf015)
     (:file                 #\uf016)
     (:time                 #\uf017)
     (:road                 #\uf018)
     (:download-alt         #\uf019)
     (:download             #\uf01a)
     (:upload               #\uf01b)
     (:inbox                #\uf01c)
     (:play-circle          #\uf01d)
     (:repeat               #\uf01e)
     (:refresh              #\uf021)
     (:list-alt             #\uf022)
     (:lock                 #\uf023)
     (:flag                 #\uf024)
     (:headphones           #\uf025)
     (:volume-off           #\uf026)
     (:volume-down          #\uf027)
     (:volume-up            #\uf028)
     (:qrcode               #\uf029)
     (:barcode              #\uf02a)
     (:tag                  #\uf02b)
     (:tags                 #\uf02c)
     (:book                 #\uf02d)
     (:bookmark             #\uf02e)
     (:print                #\uf02f)
     (:camera               #\uf030)
     (:font                 #\uf031)
     (:bold                 #\uf032)
     (:italic               #\uf033)
     (:text-height          #\uf034)
     (:text-width           #\uf035)
     (:align-left           #\uf036)
     (:align-center         #\uf037)
     (:align-right          #\uf038)
     (:align-justify        #\uf039)
     (:list                 #\uf03a)
     (:indent-left          #\uf03b)
     (:indent-right         #\uf03c)
     (:facetime-video       #\uf03d)
     (:picture              #\uf03e)
     (:pencil               #\uf040)
     (:map-marker           #\uf041)
     (:adjust               #\uf042)
     (:tint                 #\uf043)
     (:edit                 #\uf044)
     (:share                #\uf045)
     (:check                #\uf046)
     (:move                 #\uf047)
     (:step-backward        #\uf048)
     (:fast-backward        #\uf049)
     (:backward             #\uf04a)
     (:play                 #\uf04b)
     (:pause                #\uf04c)
     (:stop                 #\uf04d)
     (:forward              #\uf04e)
     (:fast-forward         #\uf050)
     (:step-forward         #\uf051)
     (:eject                #\uf052)
     (:chevron-left         #\uf053)
     (:chevron-right        #\uf054)
     (:plus-sign            #\uf055)
     (:minus-sign           #\uf056)
     (:remove-sign          #\uf057)
     (:ok-sign              #\uf058)
     (:question-sign        #\uf059)
     (:info-sign            #\uf05a)
     (:screenshot           #\uf05b)
     (:remove-circle        #\uf05c)
     (:ok-circle            #\uf05d)
     (:ban-circle           #\uf05e)
     (:arrow-left           #\uf060)
     (:arrow-right          #\uf061)
     (:arrow-up             #\uf062)
     (:arrow-down           #\uf063)
     (:share-alt            #\uf064)
     (:resize-full          #\uf065)
     (:resize-small         #\uf066)
     (:plus                 #\uf067)
     (:minus                #\uf068)
     (:asterisk             #\uf069)
     (:exclamation-sign     #\uf06a)
     (:gift                 #\uf06b)
     (:leaf                 #\uf06c)
     (:fire                 #\uf06d)
     (:eye-open             #\uf06e)
     (:eye-close            #\uf070)
     (:warning-sign         #\uf071)
     (:plane                #\uf072)
     (:calendar             #\uf073)
     (:random               #\uf074)
     (:comment              #\uf075)
     (:magnet               #\uf076)
     (:chevron-up           #\uf077)
     (:chevron-down         #\uf078)
     (:retweet              #\uf079)
     (:shopping-cart        #\uf07a)
     (:folder-close         #\uf07b)
     (:folder-open          #\uf07c)
     (:resize-vertical      #\uf07d)
     (:resize-horizontal    #\uf07e)
     (:bar-chart            #\uf080)
     (:twitter-sign         #\uf081)
     (:facebook-sign        #\uf082)
     (:camera-retro         #\uf083)
     (:key                  #\uf084)
     (:cogs                 #\uf085)
     (:comments             #\uf086)
     (:thumbs-up            #\uf087)
     (:thumbs-down          #\uf088)
     (:star-half            #\uf089)
     (:heart-empty          #\uf08a)
     (:signout              #\uf08b)
     (:linkedin-sign        #\uf08c)
     (:pushpin              #\uf08d)
     (:external-link        #\uf08e)
     (:signin               #\uf090)
     (:trophy               #\uf091)
     (:github-sign          #\uf092)
     (:upload-alt           #\uf093)
     (:lemon                #\uf094)
     (:phone                #\uf095)
     (:check-empty          #\uf096)
     (:bookmark-empty       #\uf097)
     (:phone-sign           #\uf098)
     (:twitter              #\uf099)
     (:facebook             #\uf09a)
     (:github               #\uf09b)
     (:unlock               #\uf09c)
     (:credit-card          #\uf09d)
     (:rss                  #\uf09e)
     (:hdd                  #\uf0a0)
     (:bullhorn             #\uf0a1)
     (:bell                 #\uf0a2)
     (:certificate          #\uf0a3)
     (:hand-right           #\uf0a4)
     (:hand-left            #\uf0a5)
     (:hand-up              #\uf0a6)
     (:hand-down            #\uf0a7)
     (:circle-arrow-left    #\uf0a8)
     (:circle-arrow-right   #\uf0a9)
     (:circle-arrow-up      #\uf0aa)
     (:circle-arrow-down    #\uf0ab)
     (:globe                #\uf0ac)
     (:wrench               #\uf0ad)
     (:tasks                #\uf0ae)
     (:filter               #\uf0b0)
     (:briefcase            #\uf0b1)
     (:fullscreen           #\uf0b2)
     (:group                #\uf0c0)
     (:link                 #\uf0c1)
     (:cloud                #\uf0c2)
     (:beaker               #\uf0c3)
     (:cut                  #\uf0c4)
     (:copy                 #\uf0c5)
     (:paper-clip           #\uf0c6)
     (:save                 #\uf0c7)
     (:sign-blank           #\uf0c8)
     (:reorder              #\uf0c9)
     (:list-ul              #\uf0ca)
     (:list-ol              #\uf0cb)
     (:strikethrough        #\uf0cc)
     (:underline            #\uf0cd)
     (:table                #\uf0ce)
     (:magic                #\uf0d0)
     (:truck                #\uf0d1)
     (:pinterest            #\uf0d2)
     (:pinterest-sign       #\uf0d3)
     (:google-plus-sign     #\uf0d4)
     (:google-plus          #\uf0d5)
     (:money                #\uf0d6)
     (:caret-down           #\uf0d7)
     (:caret-up             #\uf0d8)
     (:caret-left           #\uf0d9)
     (:caret-right          #\uf0da)
     (:columns              #\uf0db)
     (:sort                 #\uf0dc)
     (:sort-down            #\uf0dd)
     (:sort-up              #\uf0de)
     (:envelope-alt         #\uf0e0)
     (:linkedin             #\uf0e1)
     (:undo                 #\uf0e2)
     (:legal                #\uf0e3)
     (:dashboard            #\uf0e4)
     (:comment-alt          #\uf0e5)
     (:comments-alt         #\uf0e6)
     (:bolt                 #\uf0e7)
     (:sitemap              #\uf0e8)
     (:umbrella             #\uf0e9)
     (:paste                #\uf0ea)
     (:lightbulb            #\uf0eb)
     (:exchange             #\uf0ec)
     (:cloud-download       #\uf0ed)
     (:cloud-upload         #\uf0ee)
     (:user-md              #\uf0f0)
     (:stethoscope          #\uf0f1)
     (:suitcase             #\uf0f2)
     (:bell-alt             #\uf0f3)
     (:coffee               #\uf0f4)
     (:food                 #\uf0f5)
     (:file-alt             #\uf0f6)
     (:building             #\uf0f7)
     (:hospital             #\uf0f8)
     (:ambulance            #\uf0f9)
     (:medkit               #\uf0fa)
     (:fighter-jet          #\uf0fb)
     (:beer                 #\uf0fc)
     (:h-sign               #\uf0fd)
     (:plus-sign-alt        #\uf0fe)
     (:double-angle-left    #\uf100)
     (:double-angle-right   #\uf101)
     (:double-angle-up      #\uf102)
     (:double-angle-down    #\uf103)
     (:angle-left           #\uf104)
     (:angle-right          #\uf105)
     (:angle-up             #\uf106)
     (:angle-down           #\uf107)
     (:desktop              #\uf108)
     (:laptop               #\uf109)
     (:tablet               #\uf10a)
     (:mobile-phone         #\uf10b)
     (:circle-blank         #\uf10c)
     (:quote-left           #\uf10d)
     (:quote-right          #\uf10e)
     (:spinner              #\uf110)
     (:circle               #\uf111)
     (:reply                #\uf112)
     (:github-alt           #\uf113)
     (:folder-close-alt     #\uf114)
     (:folder-open-alt      #\uf115)))

  (defparameter *icon-names* (mapcar #'car *icon-index*)))

(defun icon-index-from-name (name)
  (or (second (assoc name *icon-index* :test #'string-equal))
      (error "Invalid icon ~S." name)))

(defun print-icon (icon stream)
  (format stream "<font face='FontAwesome'>&#~A;</font>"
          (char-code (icon-index-from-name icon))))

(defmacro icon (icon)
  `(print-icon ,icon stream))
