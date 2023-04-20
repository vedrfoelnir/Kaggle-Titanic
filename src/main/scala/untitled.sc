val config=Map[String, Any]("port"->1000, "address"->"superservice@htw", "delay" -> 5.5)
val badConfig=Map[String, Any]("address"->"superservice@htw", "delay" -> 5.5)

// .asInstanceOf[Int]

val port1:Int = config("port").asInstanceOf[Int]
try {
  val port1:Int = badConfig("port").asInstanceOf[Int]
} catch {
  case i: Exception => print(i.getMessage)
}

val port2:Int = config.get("port").get.asInstanceOf[Int]
try {
  val port2:Int = badConfig.get("port").get.asInstanceOf[Int]
} catch {
  case i: Exception => print(i.getMessage)
}

val port2:Int = badConfig.get("port").getOrElse(-1).asInstanceOf[Int]

val calories: List[(String, String, List[(String, Int)])] =
  List(("Donald Duck", "2022-01-01",List(("Frühstück",800), ("Mittag", 700), ("Snack",200), ("Abendbrot", 500))), ("Donald Duck", "2022-01-02",List(("Frühstück",700), ("Mittag", 650), ("Abendbrot", 520))), ("Donald Duck", "2022-01-03",List(("Frühstück",800), ("Mittag", 700), ("Snack",200), ("Abendbrot", 500), ("Snack",150))),("Donald Duck", "2022-01-04",List(("Frühstück",850), ("Mittag", 900), ("Snack",500), ("Snack", 400))),("Donald Duck", "2022-01-05",List(("Frühstück",600), ("Mittag", 700), ("Snack",200), ("Abendbrot", 100))), ("Dagobert Duck", "2022-01-01",List(("Frühstück",300), ("Mittag", 500), ("Snack",100), ("Abendbrot", 200))), ("Dagobert Duck", "2022-01-02", List( ("Frühstück",200), ("Mittag", 300), ("Snack",400), ("Abendbrot", 200))), ("Dagobert Duck", "2022-01-03",List(("Frühstück",800), ("Mittag", 700), ("Snack",200), ("Snack", 200))), ("Dagobert Duck", "2022-01-04",List(("Frühstück",200), ("Mittag", 300), ("Snack",200), ("Snack", 500))), ("Dagobert Duck", "2022-01-05",List(("Frühstück",200), ("Mittag", 700), ("Abendbrot", 500))))

// (Person, Tag, Liste der Mahlzeiten[String, Int])
// Mahlzeit: 2Tuple -> ("Frühstück", "Mittag", "Abendbrot", "Snack"), Int

// Get Calories based on meal type -> group into aggregation
def caloriesByMealNaive(l:List[(String, String, List[(String, Int)])]):Map[String,Int] =
  calories.flatMap(_._3).groupBy(X => X._1).mapValues(X => X.map(X => X._2).sum)

def averageCaloriesPerMealPerPerson(l: List[(String, String, List[(String, Int)])]):Map[String,Double] = ???

def averageCaloriesPerDayAndPerson(l: List[(String, String, List[(String, Int)])]):Map[String,Double] = ???

//def caloriesByMeal(l:List[(String, String, List[(String, Int)])]):Map[String,Int] = ???
caloriesByMealNaive(calories)

averageCaloriesPerMealPerPerson(calories)

averageCaloriesPerDayAndPerson(calories)
