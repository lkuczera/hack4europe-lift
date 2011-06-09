package code.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import code.lib._
import java.net.URL
import Helpers._
import net.liftweb.http._
import js.JsCmds._
import js._
import SHtml._
class Index {

  object query extends RequestVar("")
  val maxResults = 100

  def search() = {
    "name=query" #> (textElem(query) ++ hidden(doSearch))
  }

  def test() = {
    "name=query" #> (textElem(query) ++ hidden(doTest))
  }
  def doTest() = {}

  def doSearch() = {

    if (query.get == "") {
      Alert("empty query")
    } else {
      val loc = S.param("location") openOr "not-defined"
      val locStr = loc.replaceAll("\\(", "").replaceAll("\\)", "").replaceAll(" ", "");
      val locNE = S.param("locationNE").map(_.replaceAll("\\(", "").replaceAll("\\)", "").replaceAll(" ", "")) openOr "not-defined"
      val locSW = S.param("locationSW").map(_.replaceAll("\\(", "").replaceAll("\\)", "").replaceAll(" ", "")) openOr "not-defined"

      println
      println("NE: " + locNE)
      println("SW: " + locSW)
      queryEuropeanaLoc(sw = locSW, ne = locNE)
      //      queryEuropeana("http://api.europeana.eu/api/opensearch.rss?searchTerms=" + query + "&wskey=EPUGMNOLEH")
    }
  }

  def queryPlaces(location: String) = {
    val placesUrl = "https://maps.googleapis.com/maps/api/place/search/xml?location=%s&radius=500&name=%s&sensor=false&key=AIzaSyAoYgGSgry03tvLfGr5Q98MFXAXvz3R0Ks"
    val url = placesUrl.format(location, query)
    val xml = XML.load(new java.net.URL(url))
    val results = xml \\ "result"
    println("trying:" + url)
    println("results")
    println(results)
    Alert(results.mkString)
  }

  def queryEuropeana(queryUrl: String) = {
    println("querying url:" + queryUrl)
    val euSearchUrl = new URL(queryUrl)
    val euResults = XML.load(euSearchUrl)

    def additionalResults(pages: Int): NodeSeq = {
      var result = NodeSeq.Empty
      if (pages <= 1) result
      else {
        println("querying additional pages")
        println("queryUrl: " + queryUrl)
        val euSearchUrlPaginate = new URL(queryUrl + "&startPage=%d".format(pages))
        XML.load(euSearchUrl) ++ additionalResults(pages - 1)
      }
    }
    val total = (euResults \\ "totalResults").text.toInt
    val itemsPerPage = (euResults \\ "itemsPerPage").text.toInt
    val pages = total / itemsPerPage + (if ((total % itemsPerPage) == 0) 0 else 1)
    val maxPages = maxResults / itemsPerPage + (if ((maxResults % itemsPerPage) == 0) 0 else 1)
    println("euResults.size: " + euResults.size)
    val allResults = euResults ++ additionalResults(maxPages)
    println("total: " + total + "max pages: " + maxPages + " items/page: " + itemsPerPage)
    println("results.size: " + allResults.size)
    allResults
  }

  def queryEuropeanaLoc(sw: String, ne: String) = {
    val lat1 = sw.split(",")(0)
    val lat2 = ne.split(",")(0)
    val lng1 = sw.split(",")(1)
    val lng2 = ne.split(",")(1)

    val url = "http://api.europeana.eu/api/opensearch.rss?searchTerms=enrichment_place_latitude%3A[" +
      lat1 + "+TO+" + lat2 +
      "]+AND+enrichment_place_longitude%3A[" + lng1 + "+TO+" + lng2 + "]+AND+" + query + "&wskey=EPUGMNOLEH"
    val results = queryEuropeana(url)
    resultsToMarkers(results)
  }
  
  def resultsToMarkers(results: NodeSeq) = {
    val res = results.flatMap(_ \\ "item").map(_ \\ "item").map(item => {
      val lat = (item \ "place_latitude").text
      val lng = (item \ "place_longitude").text
      val url = ((item \ "enclosure") \ "@url").text
      val title = (item \ "title").text
      Marker(lat=lat, lng=lng, icon=url, title=title)
    })
    
    println("res.size" + res.size)
    println("results.size" + results.size)
    // clear markers and put new from results
    val runs = Run("map.clearMarkers()") +: (res.map(marker => 
      Run("addMarker(%s, %s, '%s', '%s')".format(marker.lat, marker.lng, "title", marker.icon))))
    runs.foreach(println)
    if (runs.size > 1) runs.reduce((cmd1: JsCmd, cmd2: JsCmd) => cmd1 & cmd2)
    else runs.head
  }
  
  def resultsToMarkers(results: NodeSeq, lat: String, lng: String) = {
    val res = results.flatMap(_ \\ "item").map(_ \\ "item").map(item => {
//      val lat = (item \ "place_latitude").text
//      val lng = (item \ "place_longitude").text
      val url = ((item \ "enclosure") \ "@url").text
      val title = (item \ "title").text
      Marker(lat=lat, lng=lng, icon=url, title=title)
    })
    
    println("res.size" + res.size)
    println("results.size" + results.size)
    // clear markers and put new from results
    val runs = Run("map.clearMarkers()") +: (res.map(marker => 
      Run("addMarker(%s, %s, '%s', '%s')".format(lat, lng, "title", marker.icon))))
    runs.foreach(println)
    println(lat)
    println(lng)
    if (runs.size > 1) runs.reduce((cmd1: JsCmd, cmd2: JsCmd) => cmd1 & cmd2)
    else runs.head
  }
  
  def placesSearch() = {
    "name=query" #> (textElem(query) ++ hidden(doPlacesSearch))
  }
  
  def doPlacesSearch() = {
    val city = java.net.URLEncoder.encode(S.param("city") openOr "")
    val lat = S.param("lat") openOr ""
    val lng = S.param("lng") openOr ""
    val url = "http://api.europeana.eu/api/opensearch.rss?searchTerms=%s+%s&wskey=EPUGMNOLEH".format(city, java.net.URLEncoder.encode(query))
    resultsToMarkers(queryEuropeana(url), lat, lng)
  }
}

case class Marker(val lat: String, val lng: String, val title: String, val icon: String)