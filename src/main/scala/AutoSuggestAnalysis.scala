import java.net.URLEncoder

import net.liftweb.json.JsonParser
import scala.io.Source
import java.io.File
import java.io.PrintWriter

object AutoSuggestAnalysis
{

	implicit class ConvertBoolToInt(booleanValue: Boolean) {
		implicit def convertToInt: Int = if (booleanValue) 1 else 0
	}

	implicit class ListSum(list: List[List[Int]]){
		implicit def listSum: Int = list.foldLeft(0){(acc,eachList) => acc + eachList.contains(1).convertToInt }
	}

	implicit class ListOptions(list: List[String])
	{
		implicit def stripList: String = if(list.isEmpty) "Nil" else list.toString().stripPrefix("List(").stripSuffix(")")

		implicit def tokenize: List[String] =
			list.foldLeft(List[String]())((acc, phrase) => phrase.substring(phrase.indexOf(" ")+1,phrase.length).split("[^A-Za-z0-9]").toList ::: acc).distinct
	}

	val writer = new PrintWriter(new File("src/main/resources/report.txt"))

	def main(args: Array[String]) {

		val searchPhraseList: List[String] = GeneratePopularPhrases.getSearchPhrases()
		println("Search Phrases : "+searchPhraseList)
		println("Search phrases size : "+searchPhraseList.size)
		try {
			writer.write("||Search phrase||Indix auto suggest list||Amazon auto suggest list||Percentage match\n")
			val sumPercent = searchPhraseList.foldLeft(0)((accPercent, phrase) => accPercent + findPercentageMatch(phrase).toInt)
			writer.write("Average match percentage : " + sumPercent / searchPhraseList.size+"%")
		}
		finally{
			writer.close()
		}
	}

	def findPercentageMatch(searchPhrase: String): Double = {
		val amazonPhraseList = amazonSuggest(searchPhrase)
		val indixPhraseList = indixSuggest(searchPhrase).take(amazonPhraseList.size)

		val indixSuggestionTokens = indixPhraseList.tokenize
		val amazonSuggestionTokens = amazonPhraseList.tokenize

		val percentageMatch = (indixSuggestionTokens.map(ind => amazonSuggestionTokens.map(amz => amz.contains(ind).convertToInt)).listSum +
			amazonSuggestionTokens.map(amz => indixSuggestionTokens.map(ind => ind.contains(amz).convertToInt)).listSum -
			indixSuggestionTokens.intersect(amazonSuggestionTokens).size).toDouble /indixSuggestionTokens.size.toDouble * 100

		writer.write("|"+searchPhrase+"|"+indixPhraseList.stripList+"|"+amazonPhraseList.stripList+"|"+percentageMatch+"%|\n")

		println(("|"+searchPhrase+"|"+indixPhraseList.stripList+"|"+amazonPhraseList.stripList+"|"+percentageMatch+"|\n"))

		percentageMatch
	}

	def indixSuggest(searchPhrase: String): List[String] ={

		val url = "https://api.indix.com/v2/products/suggestions?countryCode=US&q=" + URLEncoder.encode(searchPhrase, "UTF-8") + "&app_id=xxx&app_key=yyy"
		val indixResponseJsonString = Source.fromURL(url).mkString
		val json = JsonParser.parse(indixResponseJsonString)
		val phrases = (json \\ "suggestion").children

		phrases.map(x=>x.values.toString)
	}

	def amazonSuggest(searchPhrase: String): List[String] = {

		val url = "http://completion.amazon.com/search/complete?method=completion&mkt=1&client=amazon-search-ui&x=String&search-alias=aps&q=" + URLEncoder.encode(searchPhrase, "UTF-8") + "&qs=&cf=1&noCacheIE=1441781628953&fb=1&sc=1&" /*,"UTF-8")*/
		val amazonResponse = Source.fromURL(url).mkString
		Thread.sleep(4000)
		val phrasesBeginIndex = amazonResponse.indexOf(',') + 2
		val phrasesEndIndex = amazonResponse.indexOf(']')

		amazonResponse.substring(phrasesBeginIndex, phrasesEndIndex).replaceAll("\"", "").split(",").toList
	}
}
