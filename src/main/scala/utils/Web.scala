package utils

import java.net.URLEncoder

object Web {

  def parseQuery(query: String): Map[String, String] =
    query.split("&").flatMap(_.split("=")).grouped(2).flatMap {
      case g if g.head.isEmpty => None
      case g if g.size == 2 => Some(g.head -> g.last)
      case _ => None
    }.toMap

  def profileFor(emailAddress: String): String =
    s"email=${URLEncoder.encode(emailAddress, "UTF-8")}&uid=10&role=user".replaceAll("%40", "@")
}
