(ns apextools.jaxb
  (:import [javax.xml.bind JAXBContext Marshaller]
           [javax.xml.transform.stream StreamResult StreamSource]
           [java.io FileInputStream FileOutputStream]
           [java.nio.file Path]
           [com.sforce.soap._2006._04.metadata ObjectFactory]))


(def jaxb-context (JAXBContext/newInstance (into-array Class [ObjectFactory])))

(defn marshall
  [path element]
  (let [os (FileOutputStream. (.toFile path))]
    (doto (.createMarshaller jaxb-context)
      (.setProperty Marshaller/JAXB_FORMATTED_OUTPUT true)
      (.marshall element (StreamResult. os)))
    (.close os)))

(defn unmarshall
  [path type]
  (let [is (FileInputStream. (.toFile path))
        result (-> (.createUnmarshaller jaxb-context) (.unmarshal (StreamSource. is) type))]
    (.close is)
    result))



