package titanic

object TitanicDataSet {

  /**
   * Creates a model that predicts 1 (survived) if the person of the certain record
   * is female and 0 (deceased) otherwise
   *
   * @return The model represented as a function
   */
  def simpleModel:(Map[String, Any], String) => (Any, Any) = {
    (map, id_key) => (map(id_key), predictBasedOngender(map-id_key))
  }

  def predictBasedOngender(record: Map[String, Any]): Any = {
     record.filterKeys(_=="sex")("sex") match {
      case "female" => 1;
      case "male" => 0;
    }
  }

  /**
   * This function should count for a given attribute list, how often an attribute is
   * not present in the data records of the data set
   *
   * @param data The DataSet where the counting takes place
   * @param attList List of attributes where the missings should be counted
   * @return A Map that contains the attribute names (key) and the number of missings (value)
   */
  def countAllMissingValues(data: List[Map[String, Any]], attList: List[String]): Map[String, Int] =
    attList.map(X => X -> data.filterNot(_.contains(X)).size).toMap



  /**
   * This function should extract a set of given attributes from a record
   *
   * @param record  Record that should be extracted
   * @param attList List of attributes that should be extracted
   * @return A Map that contains only the attributes that should be extracted
   *
   */
  def extractTrainingAttributes(record:Map[String, Any], attList:List[String]):Map[String, Any]=
    record.filter(X => attList.contains(X._1))


  /**
   * This function should create the training data set. It extracts the necessary attributes,
   * categorize them and deals with the missing values. You could find some hints in the description
   * and the lectures
   *
   * @param data Training Data Set that needs to be prepared
   * @return Prepared Data Set for using it with Naive Bayes
   */
  def createDataSetForTraining(data:List[Map[String, Any]]): List[Map[String, Any]] = {
    val attList:List[String] = List("passengerID","sex","age","survived","pclass")
    data
      .map(X => extractTrainingAttributes(X, attList))
      .map(X => X.updated("age", X.getOrElse("age", 29).toString.toDouble match {
        case i if 0<i && i<=20 => "child"
        case i if 20<i && i<=30 => "young adult"
        case i if 30<i && i<=50 => "adult"
        case i if 50<i  => "old"
        case _ => "young adult"
      }))
  }

  /**
   * This function builds the model. It is represented as a function that maps a data record
   * and the name of the id-attribute to the value of the id attribute and the predicted class
   * (similar to the model building process in the train example)
   *
   * @param trainDataSet  Training Data Set
   * @param classAttrib name of the attribute that contains the class
   * @return A tuple consisting of the id (first element) and the predicted class (second element)
   */
  def createModelWithTitanicTrainingData(tdata:List[Map[String,Any]], classAttr:String):
     (Map[String, Any], String) => (Any, Any)= {
    val classVals= NaiveBayes.countAttributeValues(tdata,classAttr)
    val aValues = NaiveBayes.getAttributeValues(tdata).asInstanceOf[ Map[String, Set[Any]]]
    val data: Map[Any, Set[(String, Map[Any, Int])]] = NaiveBayes.calcAttribValuesForEachClass(tdata,classAttr)
    val condProp = NaiveBayes.calcConditionalPropabilitiesForEachClassWithSmoothing(data,aValues,classVals)
    val prior= NaiveBayes.calcPriorPropabilities(tdata,classAttr)
    (map,id_key) => (map(id_key), NaiveBayes.findBestFittingClass(NaiveBayes.calcClassValuesForPrediction(map-id_key,condProp,prior)))
  }
}