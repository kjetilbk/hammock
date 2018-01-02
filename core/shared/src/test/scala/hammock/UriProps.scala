package hammock

import atto._
import Atto._
import cats.implicits._
import org.scalacheck._
import org.scalacheck.Prop._

object UriProps extends Properties("Uri") {
  import Uri._

  val octet: Gen[Int] = Gen.choose(0, 255)

  val ipv4Gen = for {
    a <- octet
    b <- octet
    c <- octet
    d <- octet
  } yield Host.IPv4(a, b, c, d)

  val ipv6GroupGen = for {
    a <- octet
    b <- octet
  } yield Host.IPv6Group(Vector(a.toByte, b.toByte))

  val ipv6Gen = for {
    a <- ipv6GroupGen
    b <- ipv6GroupGen
    c <- ipv6GroupGen
    d <- ipv6GroupGen
    e <- ipv6GroupGen
    f <- ipv6GroupGen
    g <- ipv6GroupGen
    h <- ipv6GroupGen
  } yield Host.IPv6(a, b, c, d, e, f, g, h)

  val nonEmptyString = Gen.alphaNumStr.suchThat(!_.isEmpty)

  val otherGen = nonEmptyString.map(Host.Other)

  val localHostGen = Gen.const(Host.Localhost)

  val hostGen: Gen[Host] = Gen.oneOf(ipv4Gen, ipv6Gen, otherGen, localHostGen)

  val queryParamGen: Gen[(String, String)] = for {
    k <- nonEmptyString
    v <- nonEmptyString
  } yield (k, v)

  val authorityGen: Gen[Authority] = for {
    user <- Gen.option(nonEmptyString)
    host <- hostGen
    port <- Gen.option(Gen.choose(0L, 65536L))
  } yield Authority(user, host, port)


  val uriGen: Gen[Uri] = for {
    scheme <- Gen.some(Gen.oneOf("http", "https", "ftp", "ws"))
    authority <- Gen.some(authorityGen)
    path <- Gen.alphaNumStr.map(_.mkString("/", "/", ""))
    query <- Gen.mapOf(queryParamGen)
    fragment <- Gen.option(nonEmptyString)
  } yield Uri(scheme, authority, path, query, fragment)

  property("authority roudtrip") = forAll(authorityGen) { auth: Authority =>
    Uri.Authority.authorityParser.parseOnly(auth.show).either == Right(auth)
  }

  property("uri roudtrip") = forAll(uriGen) { uri: Uri =>
    Uri.unsafeParse(uri.show) == uri
  }
}
