package rewera.livingwithcats.functors.codecs

import rewera.livingwithcats.functors.codecs.Codec.{decode, encode}
import rewera.livingwithcats.functors.codecs.CodecInstances._
import rewera.livingwithcats.models.Box

object CodecMain {

  def main(args: Array[String]): Unit = {
    println(encode(123.4))
    println(decode[Double]("123.4"))

    println(encode(Box(123.4)))
    println(decode[Box[Double]]("123.4"))
  }
}
