/* -
 * Case Classy [case-classy-config-typesafe]
 */

package classy

import com.typesafe.config.Config

import core.Read.{ instance ⇒ read }

package object config {

  type ConfigDecoder[A] = core.Decoder[Config, A]
  type ReadConfig[A] = core.Read[Config, A]

  object ConfigDecoder {
    def apply[A](implicit ev: ConfigDecoder[A]): ConfigDecoder[A] = ev
  }

  object ReadConfig {
    def apply[A](implicit ev: ReadConfig[A]): ReadConfig[A] = ev
    def apply[A](key: String)(implicit read: ReadConfig[A]): ConfigDecoder[A] =
      read(key)
  }

  // implicit proxies for the defaults for generic derivation
  // format: OFF
  import ConfigDecoders.std._
  implicit val defaultConfigReadConfig      = read(config)
  implicit val defaultConfigReadString      = read(string)
  implicit val defaultConfigReadNumber      = read(number)      //#=typesafe
  implicit val defaultConfigReadBoolean     = read(boolean)
  implicit val defaultConfigReadInt         = read(int)
  implicit val defaultConfigReadLong        = read(long)        //#=typesafe
  implicit val defaultConfigReadDouble      = read(double)

  implicit val defaultConfigReadConfigList  = read(configList)  //#=typesafe
  implicit val defaultConfigReadStringList  = read(stringList)
  implicit val defaultConfigReadNumberList  = read(numberList)  //#=typesafe
  implicit val defaultConfigReadBooleanList = read(booleanList) //#=typesafe
  implicit val defaultConfigReadIntList     = read(intList)     //#=typesafe
  implicit val defaultConfigReadLongList    = read(longList)    //#=typesafe
  implicit val defaultConfigReadDoubleList  = read(doubleList)  //#=typesafe
}
