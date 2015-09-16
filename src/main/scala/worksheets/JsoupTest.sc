import org.jsoup.Jsoup
import scala.collection.JavaConversions._

val doc = Jsoup.connect("http://www.amazon.com/Best-Sellers-Electronics/zgbs/electronics/").get();
Thread.sleep(3000)
val allImages = doc.select(".zg_title").select("a[href]").toList

allImages.size
//allImages.map( link => link.text() )

