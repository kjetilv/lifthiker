package code.comet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.JsonCmd
import net.liftweb.http.js.JE.{JsRaw, AnonFunc, Call, JsVar}
import net.liftweb.http.CometActor
import net.liftweb.http.js.JsCmds._
import com.ibm.util.CoordinateConversion
import java.net.{URLConnection, URI}
import io.{BufferedSource, Source}
import code.model.{WalkingDistance, Position}

class GeoComet extends CometActor {

  private var position: Option[Position] = None 

  private var walkingDistance = 1200
  
  private var hits = 10
  
  private def setPosition(map: Map[String, _]) {
    position = Some(Position(
      get(map, "latitude").get,
      get(map, "longitude").get,
      get(map, "altitude")))
  }

  def get(map: Map[String, _], key: String) = 
    map.get(key) match {
      case Some(value: Double) => Some(value)
      case _ => None
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

  private def getLatitude = <span>{position.map(_.latitude).getOrElse(-1d)}</span>
  
  private def getLongitude= <span>{position.map(_.longitude).getOrElse(-1d)}</span>

  private def getAltitude = <span>{position.map(_.altitude).getOrElse(-1d)}</span>
  
  private def computeStops() {
    val response = position.map(APIClient.getStops(_, WalkingDistance(walkingDistance), hits)).getOrElse("")
    println(response)
  }
}
