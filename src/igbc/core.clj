(ns igbc.core
  (require [clj-webdriver.taxi :as taxi]
           [fs.core :as fs]
           [conch.core :as sh]
;           [incanter/incanter-excel "1.4.1" :as excel]
           [clj-webdriver.firefox :as ff]))


(def file-op "contents.txt")

(defn substring?
  "Checks for substring returns boolean value,
true if a substring."
  [sub string]
  (> (.indexOf (clojure.string/lower-case string)
               (clojure.string/lower-case sub)) -1))

(def path-firebug "/ff-extensions/firebug@software.joehewitt.com.xpi")

(defn run-cmd
  "Runs the given command and returns output.
   For example whoami, pwd.
   Optionally removes the newline appended to the output. Good for whoami, pwd etc commands."
  [cmd remove-newline?]
  (let [output (sh/stream-to-string (sh/proc cmd) :out)]
    (if (true? remove-newline?)
      (clojure.string/replace output "\n" "")
      output)))

(defn init-firefox-local-driver
  "Initializes Firefox driver.
   And sets a global implicit-wait"
  [wait-timeout]
  (taxi/set-driver! {:browser-name :firefox
                     :profile (doto (ff/new-profile)
                                (ff/set-preferences {"app.update.enabled" false})
                                (ff/set-preferences {"extensions.firebug.showFirstRunPage" false})
                                (ff/enable-extension (str (run-cmd "pwd" true) path-firebug)))})
  (taxi/implicit-wait wait-timeout))


(defn init
  "Initializes firefox and loads the site"
  []
  (init-firefox-local-driver 3000)
  (taxi/to "http://www.igbc.in/site/igbcdir/compind.jsp#mydiv4"))



;(def all-unclean-links1 (taxi/elements "#container #main_body table[style='margin-left:35px;'] a"))
;(def all-unclean-links2 (remove #(substring? "mailto:" (taxi/attribute %1 :Href)) all-unclean-links1))
;(def all-links (remove #(= "" (taxi/attribute %1 :text)) all-unclean-links2))
;(def t (taxi/elements "#facebox .body .content div tr")) ;temp object to test on
#_(clojure.string/trim (second (clojure.string/split (taxi/text (nth t 0)) #"»"))) ;This is to get the second category

(defn get-category
  "Returns the Second category. Ignores any other lines."
  [catg]
  (clojure.string/trim (second (clojure.string/split
                                (taxi/text catg)   #"»"))))

(defn extract-str
  "Returns the address"
  [elem regex]
  (clojure.string/replace (taxi/text elem) regex ""))

(defn is-categ?
  [elem]
  (substring? "»" (taxi/text elem)))

(defn key-exists?
  [regex elem]
   (not (nil? (re-seq regex (taxi/text elem)))))


(defn get-match
  [m elem]
  (cond
    (is-categ? elem) (assoc m :category (get-category elem))
    (key-exists? #"^Address : " elem) (assoc m :address (extract-str elem #"^Address : "))
    (key-exists? #"^Telephone : " elem) (assoc m :telephone (extract-str elem #"^Telephone : "))
    (key-exists? #"^Website : " elem) (assoc m :website (extract-str elem #"^Website : "))
    (key-exists? #"^Fax : " elem) (assoc m :fax (extract-str elem #"^Fax : "))
    (key-exists? #"^Hand Phone : " elem) (assoc m :hand-phone (extract-str elem #"^Hand Phone : "))
    (key-exists? #"^Email : " elem) (assoc m :email (clojure.string/split (extract-str elem #"^Email : ") #";"))
    (key-exists? #"^Contact Person : " elem) (assoc m :contact (extract-str elem #"^Contact Person : "))
    (key-exists? #"^Brief description Product" elem) m
    :else (assoc m :company-name (taxi/text elem))))

(defn get-one-data
  []
  (reduce #(get-match %1 %2) {} (taxi/elements "#facebox .body .content  table tr")))

(defn click-link
  [x]
  (taxi/click x)
  (get-one-data))

;Category,Name,Address,Telephone,Hand phone,Fax,Website,Email,Contact Person

(defn write-one-line
  [m email]
  (spit file-op (str (get m :category "") " -::- ") :append true)
  (spit file-op (str (get m :company-name "") " -::- ") :append true)
  (spit file-op (str (get m :address "") " -::- ") :append true)
  (spit file-op (str (get m :telephone "") " -::- ") :append true)
  (spit file-op (str (get m :hand-phone "") " -::- ") :append true)
  (spit file-op (str (get m :fax "") " -::- ") :append true)
  (spit file-op (str (get m :website "") " -::- ") :append true)
  (spit file-op (str email " -::- ") :append true)
  (spit file-op (str (get m :contact "") " -::- ") :append true)
  (spit file-op "\n" :append true))

(defn seq-over-emails
  "If there are multiple emails, duplicate data for each email one per line."
  [data-list]
  (doseq [m data-list]
    (if (nil? (get m :email))
      (write-one-line m "")
      (do
        (doseq [email (get m :email)]
          (write-one-line m email))))))




(defn write-data
  []
  (let [all-unclean-links1 (taxi/elements "#container #main_body table[style='margin-left:35px;'] a")
        all-unclean-links2 (remove #(substring? "mailto:" (taxi/attribute %1 :Href)) all-unclean-links1)
        all-links (remove #(= "" (taxi/attribute %1 :text)) all-unclean-links2)]
    (seq-over-emails (map #(click-link %) all-links))))
