val trainDataSet= List(Map[String,String]("day"-> "weekday", "season"->"spring", "wind"->"none", "rain"->"none", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"winter", "wind"->"none", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"winter", "wind"->"none", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"winter", "wind"->"high", "rain"->"heavy", "class"->"late"), Map[String,String]("day"-> "saturday", "season"->"summer", "wind"->"normal", "rain"->"none", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"autumn", "wind"->"normal", "rain"->"none", "class"->"very late"), Map[String,String]("day"-> "holiday", "season"->"summer", "wind"->"high", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "sunday", "season"->"summer", "wind"->"normal", "rain"->"none", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"winter", "wind"->"high", "rain"->"heavy", "class"->"very late"), Map[String,String]("day"-> "weekday", "season"->"summer", "wind"->"none", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "saturday", "season"->"spring", "wind"->"high", "rain"->"heavy", "class"->"cancled"), Map[String,String]("day"-> "weekday", "season"->"summer", "wind"->"high", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "saturday", "season"->"winter", "wind"->"normal", "rain"->"none", "class"->"late"), Map[String,String]("day"-> "weekday", "season"->"summer", "wind"->"high", "rain"->"none", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"winter", "wind"->"normal", "rain"->"heavy", "class"->"very late"), Map[String,String]("day"-> "saturday", "season"->"autumn", "wind"->"high", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"autumn", "wind"->"none", "rain"->"heavy", "class"->"on time"), Map[String,String]("day"-> "holiday", "season"->"spring", "wind"->"normal", "rain"->"slight", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"spring", "wind"->"normal", "rain"->"none", "class"->"on time"), Map[String,String]("day"-> "weekday", "season"->"spring", "wind"->"normal", "rain"->"slight", "class"->"on time"))

def countAttributeValues(data:List[Map[String, Any]], attribList:String): Map[Any, Int] = data.flatMap(X => X.filter(_._1 == attribList).values).groupBy(identity).mapValues(_.size)
def getAttributes(data:List[Map[String, Any]]):Set[String] = data.flatMap(X => X.keys).groupBy(identity).keys.toSet
def getAttributeValues(data:List[Map[String, Any]]):Map[String,Set[Any]] = getAttributes(data).map(a => (a, data.map(_(a)).groupBy(identity).keys.toSet)).toMap
def calcPriorPropabilities(data:List[Map[String, Any]], classAttrib:String):Map[Any,Double]= countAttributeValues(data, classAttrib).mapValues(X => X.asInstanceOf[Double]./(countAttributeValues(data, classAttrib).values.sum))
def calcConditionalPropabilitiesForEachClass(data: Map[Any, Set[(String, Map[Any, Int])]],classCounts:Map[Any,Int]): Map[Any,Set[(String, Map[Any, Double])]] = data.map(X => (X._1, X._2.map(V => V._1 -> V._2.mapValues(_.asInstanceOf[Double]/classCounts.filter(_._1==X._1).values.sum))))
def calcAttribValuesForEachClass(data:List[Map[String, Any]], classAttrib:String): Map[Any, Set[(String, Map[Any, Int])]] = countAttributeValues(data, classAttrib).map(X => X._1 -> data.filter(_(classAttrib)==X._1).flatMap(_.filterNot(_._1==classAttrib)).groupBy(_._1).map(X => (X._1, X._2.map(V => V._2))).map(X => (X._1, X._2.groupBy(identity).mapValues(X => X.size))).toSet)

val record = Map[String,String]("day"->"weekday", "season"->"winter", "wind"->"high", "rain"->"heavy")
val classVals = countAttributeValues(trainDataSet, "class")
val aValues = getAttributeValues(trainDataSet)
val data = calcAttribValuesForEachClass(trainDataSet, "class")
val conditionalProps = calcConditionalPropabilitiesForEachClass(data, classVals)
val priorProps = calcPriorPropabilities(trainDataSet, "class")

val getProps = priorProps.flatMap(X => conditionalProps.filter(_._1==X._1).mapValues(X => {for (el <- record) yield (X.map(V => if(el._1==V._1) V._2.getOrElse(el._2, 0.0))).toList}.flatten)).mapValues(_.filter(_.isInstanceOf[Double]).toSeq.toList).mapValues(X => X.fold(1.0)((k,v) => k.asInstanceOf[Double]*v.asInstanceOf[Double]).asInstanceOf[Double])
priorProps.map(X => X._1 -> X._2*getProps.get(X._1).sum)
//val cleanProps:Map[Any, Double] = getProps.mapValues(_.filter(_.isInstanceOf[Double]).toSeq.toList).mapValues(X => X.fold(1.0)((k,v) => k.asInstanceOf[Double]*v.asInstanceOf[Double]).asInstanceOf[Double])

//Map[Any,Set[(String, Map[Any, Double])]]


aValues.filterNot(_._1=="class")
val attrib = "rain"
data.mapValues(_.map(X => (X._1, X._2.updated(attrib, X._2.getOrElse(attrib, 0.0)))).toSet)
//data.map(X => (X._1, X._2.flatMap(V => Set(V._1, V._2.updated()))))
val x = 15
data
data.map(X => (X._1, X._2.map(V => V._1 -> aValues.filter(_._1==V._1).values.flatMap(_.flatMap(C => V._2.updated(C, V._2.getOrElse(C, 0)))).toMap)))