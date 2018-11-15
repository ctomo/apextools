(ns apextools.jaxb
  (:import [javax.xml.bind JAXBContext Marshaller JAXBElement]
           [javax.xml.transform.stream StreamResult StreamSource]
           [java.io FileInputStream FileOutputStream]
           [java.nio.file Path]
           [com.sforce.soap._2006._04.metadata ObjectFactory]
           [javax.xml.namespace QName]))


(def jaxb-context (JAXBContext/newInstance (into-array Class [ObjectFactory])))

(def ns-uri "http://soap.sforce.com/2006/04/metadata")

(defn marshal
  [path ^Object element]
  (let [os (FileOutputStream. (.toFile path))
        name (-> element .getClass .getSimpleName)
        qname (QName. ns-uri name)
        root (JAXBElement. qname (.getClass element) element)]
    (doto (.createMarshaller jaxb-context)
      (.setProperty Marshaller/JAXB_FORMATTED_OUTPUT true)
      (.marshal root (StreamResult. os)))
    (.close os)))

(defn unmarshal
  [path type]
  (let [is (FileInputStream. (.toFile path))
        result (-> (.createUnmarshaller jaxb-context) (.unmarshal (StreamSource. is) type))]
    (.close is)
    result))
