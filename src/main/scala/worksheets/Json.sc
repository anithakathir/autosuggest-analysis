implicit class ConvertBoolToInt(booleanValue: Boolean) {
	implicit def convertToInt: Int = if (booleanValue) 1 else 0
}

implicit class ListSum(list: List[List[Int]]){
	implicit def listSum: Int = list.foldLeft(0){(acc,eachList) => acc + eachList.contains(1).convertToInt }
}

implicit class ListOptions(list: List[String])
{
	implicit def stripList: String = if(list.isEmpty) "--" else list.toString().stripPrefix("List(").stripSuffix(")")

	implicit def tokenize: List[String] =
		list.foldLeft(List[String]())((acc, phrase) => phrase.substring(phrase.indexOf(" ")+1,phrase.length).split("[^A-Za-z0-9]").toList ::: acc).distinct
}
val indixPhraseList = List("toms women", "toms women s", "toms women classics", "toms women s classics", "toms women slip", "toms women s slip", "toms women canvas", "toms women s canvas", "toms women on", "toms women slip on")
val amazonPhraseList = List("toms wedges", "toms women", "toms womens shoes", "toms wedges shoes womens","toms ware", "toms wedge bootie", "toms ware women dress", "toms womens classic", "toms white", "toms wear")

val indp = indixPhraseList.tokenize
val amzp = amazonPhraseList.tokenize


(indp.map(ind => amzp.map(amz => amz.contains(ind).convertToInt)).listSum +
	amzp.map(amz => indp.map(ind => ind.contains(amz).convertToInt)).listSum - indp.intersect(amzp).size).toDouble /indp.size.toDouble * 100

/*
val indixPhraseList = List("toms women", "toms women s", "toms women classics", "toms women s classics")//, "toms women slip", "toms women s slip", "toms women canvas", "toms women s canvas", "toms women on", "toms women slip on")
val amazonPhraseList = List("toms wedges", "toms women", "toms womens shoes", "toms wedges shoes womens")//, "toms ware", "toms wedge bootie", "toms ware women dress", "toms womens classic", "toms white", "toms wear")
indixPhraseList.size
amazonPhraseList.size
//indixPhraseList.map(indPhrase => amazonPhraseList.foldLeft(0)((acc,amzPhrase) => acc + amzPhrase.contains(indPhrase).convert)).sum
indixPhraseList.map(indPhrase => amazonPhraseList.foldLeft(0)((acc,amzPhrase) => acc + amzPhrase.contains(indPhrase).convert)).sum

indixPhraseList.map(ind => amazonPhraseList.map(amz => amz.contains(ind).convert))//.listSum +

amazonPhraseList.map(amz => indixPhraseList.map(ind => ind.contains(amz).convert))//.listSum
//- indixPhraseList.intersect(amazonPhraseList).size

//indixPhraseList.map(ind => amazonPhraseList.map(amz => amz.contains(ind).convert)).listSum

implicit class ListSum(list: List[List[Int]]){
	implicit def listSum: Int = list.foldLeft(0){(acc,eachList) => acc + eachList.contains(1).convert }
}

implicit class Convert(bool: Boolean) {
	implicit def convert: Int = if(bool) 1 else 0
}
*/



//obj = yaml.load(ios).asInstanceOf[java.util.Map[String, Any]
/*
val file = "src/categories.yaml"
//val input = Source.fromFile(file)
val f = Source.fromFile("src/main/resources/categories")*/
