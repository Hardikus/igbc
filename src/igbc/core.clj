(ns igbc.core
  (require [clj-webdriver.taxi :as taxi]
           [fs.core :as fs]
           [conch.core :as sh]
           [clj-webdriver.firefox :as ff]))

(def file-op "contents.txt")
(def path-firebug "/ff-extensions/firebug@software.joehewitt.com.xpi")

(defn substring?
  "Checks for substring returns boolean value, true if a substring."
  [sub string]
  (> (.indexOf (clojure.string/lower-case string)
               (clojure.string/lower-case sub)) -1))

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

(defn get-category
  "Returns the Second category."
  [catg]
  (clojure.string/trim (second (clojure.string/split
                                (taxi/text catg)   #"»"))))

(defn extract-str
  "Returns the string after removing given regex as well as comma."
  [elem regex]
  (let [temp1 (clojure.string/replace (taxi/text elem) regex "")
        temp2 (clojure.string/replace temp1 #"," "")]
    temp2))

(defn is-categ?
  "Checks if given element is a category?"
  [elem]
  (substring? "»" (taxi/text elem)))

(defn key-exists?
  "Checks if given regex matches."
  [regex elem]
   (not (nil? (re-seq regex (taxi/text elem)))))

(defn get-match
  "Checks if given row has which key and associates that key val with given map.
   Ignores Brief description.
   If none matches it must be company name."
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
  "Returns a map data of one company."
  []
  (reduce #(get-match %1 %2)
          {}
          (taxi/elements "#facebox .body .content  table tr")))

(defn get-link-data
  "Clicks on the link and gets the data as a map."
  [x]
  (taxi/click x)
  (get-one-data))

(defn write-one-line
  "Writes each company data and given email address on one newline."
  [m email]
  (spit file-op (str (get m :category "") ",") :append true)
  (spit file-op (str (get m :company-name "") ",") :append true)
  (spit file-op (str (get m :address "") ",") :append true)
  (spit file-op (str (get m :telephone "") ",") :append true)
  (spit file-op (str (get m :hand-phone "") ",") :append true)
  (spit file-op (str (get m :fax "") ",") :append true)
  (spit file-op (str (get m :website "") ",") :append true)
  (spit file-op (str email ",") :append true)
  (spit file-op (str (get m :contact "") "") :append true)
  (spit file-op "\n" :append true))

(defn doseq-over-emails
  "If there are multiple emails, duplicate data for each email one per line.
   If no emails just add email as empty and add rest of the content."
  [data-list]
  (doseq [m data-list]
    (if (nil? (get m :email))
      (write-one-line m "")
      (do
        (doseq [email (get m :email)]
          (write-one-line m email))))))

(defn write-data
  "Gets all the data and writes it to a file."
  []
  (let [all-unclean-links1 (taxi/elements "#container #main_body table[style='margin-left:35px;'] a")
        all-unclean-links2 (remove #(substring? "mailto:" (taxi/attribute %1 :Href)) all-unclean-links1)
        all-links (remove #(= "" (taxi/attribute %1 :text)) all-unclean-links2)]
    (doseq-over-emails (map #(get-link-data %) all-links))))

(defn init
  "Initializes firefox, loads the site and start scraping."
  []
  (init-firefox-local-driver 3000)
  (taxi/to "http://www.igbc.in/site/igbcdir/compind.jsp#mydiv4")
  #_(write-data))
