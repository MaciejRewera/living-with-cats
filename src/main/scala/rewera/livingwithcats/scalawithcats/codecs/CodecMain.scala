package rewera.livingwithcats.scalawithcats.codecs

import rewera.livingwithcats.scalawithcats.codecs.Codec.{decode, encode}
import rewera.livingwithcats.scalawithcats.codecs.CodecInstances._
import rewera.livingwithcats.scalawithcats.models.Box

object CodecMain {

  def main(args: Array[String]): Unit = {
    println(encode(123.4))
    println(decode[Double]("123.4"))

    println(encode(Box(123.4)))
    println(decode[Box[Double]]("123.4"))
  }
}
