package code.comet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.JsonCmd
import net.liftweb.http.js.JE.{JsRaw, AnonFunc, Call, JsVar}
import net.liftweb.http.js.JsCmds._
import java.net.InetSocketAddress
import code.model.{WalkingDistance, Position}
import net.liftweb.http.{SHtml, CometActor}

class GeoComet extends CometActor {

  private var position: Option[Position] = None

  private var walkingDistance = WalkingDistance(500)

  private val hits = 5

  private val address = new InetSocketAddress(System getProperty "trafikantenapi", 80)

  private val googleMapsClient = new GoogleMapsClient(System getProperty "googleapikey")

  private val client = new TrafikantenClient(address)

  override def handleJson(in: Any): JsCmd = in match {
    case JsonCmd("updatePosition", _, map: Map[String, Map[String, _]], _) =>
      setPosition(map("coords"))
      val stops = computeStops()
      CmdPair(
        SetHtml("stops", stops),
        CmdPair(
          SetHtml("longitude", getLongitude),
          CmdPair(
            SetHtml("latitude", getLatitude),
            CmdPair(
              SetHtml("altitude", getAltitude),
              SetHtml("googlemaps-canvas", googleMapsClient.getCanvasScript(position.get, "map_canvas"))
            )
          )
        )
      )
    case _ =>
      println("Unsupported JSON call: " + in)
  }

  private val waiting = <span>Waiting ...</span>

  def render = "#geo *" #> getGeoScript &
    "#longitude *" #> getLongitude &
    "#latitude *" #> getLatitude &
    "#altitude *" #> getAltitude &
    "#stops *" #> waiting &
    "#map *" #> getMap &
    "#googlemaps-init *" #> googleMapsClient.getInitialScript &
    "#googlemaps-create *" #> googleMapsClient.getStartScript(zoom = 16) &
    "#googlemaps-canvas *" #> position.map(googleMapsClient.getCanvasScript(_, "map_canvas")).getOrElse(waiting) &
    "#walking *" #> SHtml.text("100", (string) => { setWalkingDistance(string.toInt) }) 

  private def setWalkingDistance(dist: Int) {
    walkingDistance = WalkingDistance(dist)
  }
  
  private def setPosition(map: Map[String, _]) {
    position = Some(Position(
      getArg(map, "latitude").get,
      getArg(map, "longitude").get,
      getArg(map, "altitude")))
  }

  private def getArg(map: Map[String, _], key: String) =
    map.get(key) match {
      case Some(value: Double) => Some(value)
      case _ => None
    }

  private def getGeoScript = {
    <span>
      {Script(jsonInCode)}{Script(JsIf(JsVar("navigator.geolocation"),
      Call(
        "navigator.geolocation.getCurrentPosition",
        AnonFunc(
          "coords",
          jsonCall(
            "updatePosition",
            JsRaw("coords")
          )))))}
    </span>
  }

  private def getMap =
    position.map(p => <iframe width="425"
                              height="350"
                              frameborder="0" scrolling="no" marginheight="0" marginwidth="0"
                              src={ googleMapsClient getURL p }/>).getOrElse(<span>Waiting...</span>)

  private def getLatitude = get(_.latitude)

  private def getLongitude= get(_.longitude)

  private def getAltitude = get(_.altitude)

  private def get[T](fun: Position => T) = <span>{position.map(fun).getOrElse("Waiting...")}</span>

  private def computeStops() = {
    position.map(pos => { 
      val stopList = client.getStops(pos, walkingDistance, hits)
      stopList match {
        case Nil => waiting
        case list => 
          <ul>
            { list.map (stop => 
            <li>
              { stop.Name } ({ stop.ID })
              <button onclick={ "zoomTo(" + stop.latitude + ", " + stop.longitude + ")" } type="button">
                Go to
              </button>
              <ul>
                { stop.Lines.filter(_.LineID < 50).map(line => { <li>{ 
                  <span>{ line.mode}</span>
                  <span>&nbsp;</span>
                  <span>{ line.LineName }</span>
                }</li>}) 
                }
              </ul>
            </li>) } 
          </ul>
      }
    }).getOrElse(waiting)
  }
}
