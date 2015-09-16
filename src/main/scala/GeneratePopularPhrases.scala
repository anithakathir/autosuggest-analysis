import org.jsoup.Jsoup
import scala.collection.JavaConversions._
import scala.io.Source

object GeneratePopularPhrases {
	val symbolExpansionMap = Map("\"" -> "Inch", "%" -> "Percentage")

	def getSearchPhrases(): List[String] ={

		val categoryList = Source.fromFile("src/main/resources/categories").getLines().toList

		categoryList.foldLeft(List[String]())((acc,category) => acc ::: parseAmzForPopularSearchPhrases(category))
	}

	def parseAmzForPopularSearchPhrases(categoryURL: String): List[String] = {
		val document = Jsoup.connect(categoryURL).get;
		Thread.sleep(3000)
		val productTitles = document.select(".zg_title").select("a[href]").toList
		val titleList = productTitles.map(link => link.text().replaceSymbols.removeSpecialCharacters)

		titleList.map(title => title.substring(0,title.indexOf(" ")+2)).removeMeaninglessWords.distinct
	}

	implicit class RemoveSpecialCharacters(str: String) {
		implicit def removeSpecialCharacters: String = str.replaceAll("[^A-Za-z0-9. ]"," ")
		implicit def replaceSymbols: String = symbolExpansionMap.foldLeft(str){case(text,(symbol,replacement)) => text.replace(symbol,replacement)}
	}

	implicit class RemovePrepositions(list: List[String]) {
		implicit def removeMeaninglessWords: List[String] = list.filterNot(word => word.matches(" [0-9]"))
	}
}
