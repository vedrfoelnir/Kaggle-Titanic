package titanic

// Ausführbares Programm
// zur Erzeugung eines Files zur Übermittlung an kaggle

object CreatePrediction extends App{

  // load datasets
  val trainData = Utils.loadDataCSV("train.csv")
  val test = Utils.loadDataCSV("test.csv")

  println("Train Dataset:" + trainData.size + " Elements")
  println("Test Dataset:" + test.size + " Elements")

  val train = TitanicDataSet.createDataSetForTraining(trainData)
  val model= TitanicDataSet.createModelWithTitanicTrainingData(train,"survived")
  val evaluation= TitanicDataSet.createDataSetForTraining(test)
  val evalData= evaluation.map(map=>map-("survived"))
  val prediction= NaiveBayes.applyModel(model,evalData,"passengerID")
  Utils.createSubmitFile("TitanicPrediction.txt",prediction,"passengerID,survived")
  println(prediction)
}
