import org.yaml.snakeyaml.Yaml
import scala.io.Source

object YamlTest {
	def main(args: Array[String]) {
		val file = "src/categories.yaml"
		val input = Source.fromFile(file).mkString

		val obj = (new Yaml).load(input).asInstanceOf[scala.collection.mutable.Map[String,_]]

		val list = obj.get("categories")

		println(list)
	}
}



