/* -
 * Case Classy [case-classy-tests]
 */

package classy
package generic_test_

import com.typesafe.config.ConfigFactory
import scala.Predef._

object Example1 extends App {

  case class Bar(value: String)

  case class Foo(
    a: String,
    b: Option[Int],
    c: List[String],
    bars: List[Bar]
  )

  val typesafeConfig = ConfigFactory parseString """
   | a = 1
   | c = ["hello", "world"]
   | bars = [{ value: hello }]
   |""".stripMargin

  {
    // Zed: Full Black Magic
    import classy.config._
    import classy.generic.auto._

    val res = ConfigDecoder[Foo].decode(typesafeConfig)
    println("Zed: " + res)
  }

  {
    // Yax: Partial Black Magic
    import classy.config._
    import classy.generic.deriveDecoder
    import com.typesafe.config.Config

    implicit val decodeBar = deriveDecoder[Config, Bar]
    implicit val decodeFoo = deriveDecoder[Config, Foo]

    val res = ConfigDecoder[Foo].decode(typesafeConfig)
    println("Yax: " + res)
  }

  {
    // Xan: Some Implicit Magic
    import com.typesafe.config.Config
    import classy.config._

    val decodeA = ReadConfig[String]("a")
    val decodeB = ReadConfig[Int]("b").option
    val decodeC = ReadConfig[List[String]]("c")
    val decodeBar = ReadConfig[String]("value").map(value ⇒ Bar(value))
    val decodeBars = ReadConfig[List[Config]]("bars") andThen decodeBar.sequence

    implicit val decodeFoo = (decodeA and decodeB and decodeC and decodeBars).map {
      case (((a, b), c), bar) ⇒ Foo(a, b, c, bar)
    }

    val res = ConfigDecoder[Foo].decode(typesafeConfig)
    println("Xan: " + res)
  }

  {
    // Wat: No magic
    import classy.config.ConfigDecoder
    import classy.config.ConfigDecoders.std._

    val decodeA = string("a")
    val decodeB = int("b").option
    val decodeC = stringList("c")
    val decodeBar = string("value").map(value ⇒ Bar(value))
    val decodeBars = configList("bars") andThen decodeBar.sequence

    implicit val decodeFoo = (decodeA ~ decodeB ~ decodeC ~ decodeBars).mapN(Foo)

    val res = ConfigDecoder[Foo].decode(typesafeConfig)
    println("Wat: " + res)
  }

}
