package code.comet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.JsonCmd
import net.liftweb.http.js.JE.{JsRaw, AnonFunc, Call, JsVar}
import net.liftweb.http.CometActor
import net.liftweb.http.js.JsCmds._
import com.ibm.util.CoordinateConversion
import java.net.{URLConnection, URI}
import io.{BufferedSource, Source}

class GeoComet extends CometActor {

  private var longitude = .0d

  private var latitude = .0d
  
  private var altitude = .0d

  private var walkingDistance = 1200
  
  private var hits = 10
  
  private def setPosition(map: Map[String, _]) {
    longitude = get(map, "longitude")
    latitude = get(map, "latitude")
    altitude = get(map, "altitude")
  }

  def get(map: Map[String, _], key: String) = 
    map.get(key) match {
      case Some(value: Double) => value
      case _ => -1d
    }

  override def handleJson(in: Any): JsCmd = in match {
    case JsonCmd("updatePosition", _, map: Map[String, Map[String, _]], _) =>
      setPosition(map("coords"))
      computeStops()
      CmdPair(
        SetHtml("longitude", getLongitude),
        CmdPair(
          SetHtml("latitude", getLatitude),
          SetHtml("altitude", getAltitude)
        )
      )
    case _ =>
      println("Unsupported JSON call: " + in)
  }

  def render = 
    "#geo *" #> <span>{
      Script(jsonInCode)
      } {
      Script(JsIf(JsVar("navigator.geolocation"),
        Call(
          "navigator.geolocation.getCurrentPosition",
          AnonFunc(
            "coords",
            jsonCall(
              "updatePosition",
              JsRaw("coords")
            )))))
      } </span>&
    "#longitude *" #> getLongitude &
    "#latitude *" #> getLatitude &
    "#altitude *" #> getAltitude &
    "#stops *" #> <span>Waiting ...</span>

  private def getAltitude = <span>{altitude}</span>

  private def getLatitude = <span>{latitude}</span>

  private def getLongitude = <span>{longitude}</span>
  
  private def computeStops() {
    val conversion: CoordinateConversion = new CoordinateConversion()
    val converted = conversion.latLon2UTM(latitude, longitude)
    val values = converted.split("\\s+").takeRight(2).map(_.toInt)
    val uri = URI create ("http://api-test.trafikanten.no/Place/GetClosestStopsAdvancedByCoordinates/?coordinates=" +
      "(X=" + values(0) +
      ",Y=" + values(1) +
      ")&proposals=" + hits + 
      "&walkingDistance=" + walkingDistance +
      "")
    val source: BufferedSource = Source.fromURL(uri.toURL)
    try {
      source.getLines().foreach(println _)
    } finally {
      source.close()
    }
  }
}
