val trainDataSet= List(Map[String,String]("day"-> "weekday", "season"->"spring", "wind"->"none", "rain"->"none", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"winter", "wind"->"none", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"winter", "wind"->"none", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"winter", "wind"->"high", "rain"->"heavy", "class"->"late"), Map[String,String]("day"-> "saturday", "season"->"summer", "wind"->"normal", "rain"->"none", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"autumn", "wind"->"normal", "rain"->"none", "class"->"very late"), Map[String,String]("day"-> "holiday", "season"->"summer", "wind"->"high", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "sunday", "season"->"summer", "wind"->"normal", "rain"->"none", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"winter", "wind"->"high", "rain"->"heavy", "class"->"very late"), Map[String,String]("day"-> "weekday", "season"->"summer", "wind"->"none", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "saturday", "season"->"spring", "wind"->"high", "rain"->"heavy", "class"->"cancled"), Map[String,String]("day"-> "weekday", "season"->"summer", "wind"->"high", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "saturday", "season"->"winter", "wind"->"normal", "rain"->"none", "class"->"late"), Map[String,String]("day"-> "weekday", "season"->"summer", "wind"->"high", "rain"->"none", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"winter", "wind"->"normal", "rain"->"heavy", "class"->"very late"), Map[String,String]("day"-> "saturday", "season"->"autumn", "wind"->"high", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"autumn", "wind"->"none", "rain"->"heavy", "class"->"on time"), Map[String,String]("day"-> "holiday", "season"->"spring", "wind"->"normal", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"spring", "wind"->"normal", "rain"->"none", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"spring", "wind"->"normal", "rain"->"slight", "class"->"on time"))
trainDataSet.flatMap(X => X.filter(_._1 == "class").values).groupBy(identity).mapValues(_.size)
Map("late" -> 2, "cancled" -> 1, "very late" -> 3, "on time" -> 14)
trainDataSet.flatMap(X => X.keys).groupBy(identity).keys
val attributeList = Set("season", "rain", "wind", "class", "day")
//val num = data.values.sum
//data.mapValues(X => X.asInstanceOf[Double]/20)

//val w1 = classes.map(X => Map(X -> getAttributes(trainDataSet)))
//val w2 = classes.map(X => Map(X -> getAttributeValues(trainDataSet).filter(_!="class")))
//val w3 = classes.map(X => Map(X -> getAttributes(trainDataSet).filter(_!="class").map(X => X -> countAttributeValues(trainDataSet.filter(_!=X), X).filter(_!=X))))
print("----------------")
def countAttributeValues(data:List[Map[String, Any]], attribList:String): Map[Any, Int] = data.flatMap(X => X.filter(_._1 == attribList).values).groupBy(identity).mapValues(_.size)
def getAttributes(data:List[Map[String, Any]]):Set[String] = data.flatMap(X => X.keys).groupBy(identity).keys.toSet
def getAttributeValues(data:List[Map[String, Any]]):Map[String,Set[Any]] = getAttributes(data).map(a => (a, data.map(_(a)).groupBy(identity).keys.toSet)).toMap
getAttributes(trainDataSet).map(a => (a, trainDataSet.map(_(a))))
trainDataSet.map(X => X("season"))

trainDataSet.filter(X => X.get("class").get=="late")

val classVals = countAttributeValues(trainDataSet, "class")
val attributes = getAttributes(trainDataSet)
val values = getAttributeValues(trainDataSet)

val classes = countAttributeValues(trainDataSet, "class")

def getAttributes(data:List[Map[String, Any]]):Set[String] = data.flatMap(X => X.keys).groupBy(identity).keys.toSet
def getAttributeValues(data:List[Map[String, Any]]):Map[String,Set[Any]] = getAttributes(data).map(a => (a, data.map(_(a)).groupBy(identity).keys.toSet)).toMap
def calcPriorPropabilities(data:List[Map[String, Any]], classAttrib:String):Map[Any,Double]= {
  val dataset = countAttributeValues(data, classAttrib)
  dataset.mapValues(X => X.asInstanceOf[Double]./(dataset.values.sum))
}
def calcClassValuesForPrediction(record:Map[String,Any], conditionalProps: Map[Any,Set[(String, Map[Any, Double])]], priorProps:Map[Any,Double]):Map[Any,Double]=
  priorProps.map(X => X._1 -> X._2*conditionalProps.filter(_._1==X._1).mapValues(X => {for (el <- record) yield (X.map(V => if(el._1==V._1) V._2.get(el._2)))}.map(B => B.asInstanceOf[Double])))
def calcConditionalPropabilitiesForEachClass(data: Map[Any, Set[(String, Map[Any, Int])]],classCounts:Map[Any,Int]): Map[Any,Set[(String, Map[Any, Double])]] =
  data.map(X => (X._1, X._2.map(V => V._1 -> V._2.mapValues(_.asInstanceOf[Double]/classCounts.filter(_._1==X._1).values.sum))))
def calcAttribValuesForEachClass(data:List[Map[String, Any]], classAttrib:String): Map[Any, Set[(String, Map[Any, Int])]] =
  countAttributeValues(data, classAttrib).map(X => X._1 -> data.filter(_(classAttrib)==X._1).flatMap(_.filterNot(_._1==classAttrib)).groupBy(_._1).map(X => (X._1, X._2.map(V => V._2))).map(X => (X._1, X._2.groupBy(identity).mapValues(X => X.size))).toSet)



//Map[Any, Set[(String, Map[Any, Int])]]
//val w3 = classes.map(X => Map(X -> getAttributes(trainDataSet).filter(_!="class").map(X => X -> countAttributeValues(trainDataSet.filter(_!=X), X).filter(_!=X))))
//classes.map(X => X._1 -> trainDataSet.filter(entry => entry.get("class").get==X._1).flatMap(X => X.filterNot(_._1=="class")))
//classes.map(X => X._1 -> trainDataSet.filter(entry => entry("class")==X._1).flatMap(_.filterNot(_._1=="class").map(X => (X._1, Map((X._2, 1))))))
//classes.map(X => X._1 -> trainDataSet.filter(_("class")==X._1).flatMap(_.filterNot(_._1=="class")).toSet)
classes.map(X => X._1 -> trainDataSet.filter(_("class")==X._1).flatMap(_.filterNot(_._1=="class").map(X => (X._1, Map((X._2, 1))))).groupBy(_._1))

//classes.map(X => X._1 -> trainDataSet.filter(entry => entry.get("class").get==X._1).groupBy(identity).mapValues(_.size))
//trainDataSet.flatMap(entry => entry.filter(_._1 == "class").values)
val classAttrib = "class"
//countAttributeValues(data, classAttrib).map(X => X._1 -> data.filter(_(classAttrib)==X._1).flatMap(_.filterNot(_._1==classAttrib).map(X => (X._1, Map((X._2, 1))))).groupBy(_._1).map(X => (X._1, X._2.map(V => V._2).fold(Map[Any, Int])((k, v) => ))))
countAttributeValues(trainDataSet, classAttrib).map(X => X._1 -> trainDataSet.filter(_(classAttrib)==X._1).flatMap(_.filterNot(_._1==classAttrib)).groupBy(_._1).map(X => (X._1, X._2.map(V => V._2))).map(X => (X._1, X._2.groupBy(identity).mapValues(X => X.size))).toSet)


val data = calcAttribValuesForEachClass(trainDataSet, classAttrib)
val classCounts = countAttributeValues(trainDataSet, classAttrib)
val priorProps = calcPriorPropabilities(trainDataSet, classAttrib)
countAttributeValues(data:List[Map[String, Any]], attribList:String): Map[Any, Int] = data.flatMap(X => X.filter(_._1 == attribList).values).groupBy(identity).mapValues(_.size)
val conditionalProps = calcConditionalPropabilitiesForEachClass(data, classCounts)
val record = Map[String,String]("day"->"weekday", "season"->"winter", "wind"->"high", "rain"->"heavy")
priorProps.map(X => X._1 -> X._2*(conditionalProps.filter(_._1==X._1).mapValues(SET => {
  for (map <- SET) yield map._2.get(record.values).product
})))

priorProps.map(X => conditionalProps.filter(_._1==X._1).mapValues(X => {for (el <- record) yield (X.map(V => if(el._1==V._1) V._2.get(el._2)))}.map(B => B.asInstanceOf[Double])))

//priorProps.map(X => X._1 -> X._2*conditionalProps.filter(_._1==X._1).mapValues(X => {for (el <- record) yield (X.map(V => if(el._1==V._1) V._2.get(el._2)))}.map(B => B.asInstanceOf[Double])))
classCounts.filter(_._1=="on time").values.sum
data.map(X => X._2.map(V => V._1 -> V._2.mapValues(T => (T, X._1))))