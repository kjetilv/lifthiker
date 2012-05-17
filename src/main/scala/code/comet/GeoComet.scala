package code.comet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.JsonCmd
import net.liftweb.http.js.JE.{JsRaw, AnonFunc, Call, JsVar}
import net.liftweb.http.js.JsCmds._
import java.net.InetSocketAddress
import net.liftweb.http.{SessionVar, S, SHtml, CometActor}
import net.liftweb.common.{Full, Empty, Box}
import code.model.{Stops, WalkingDistance, Position}

class GeoComet extends CometActor {

  private object route extends SessionVar[Box[Int]](Empty)

  private object position extends SessionVar[Box[Position]](Empty)

  private object walkingDistance extends SessionVar[WalkingDistance](WalkingDistance(500))

  private object stopCount extends SessionVar[Int](5)

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
              mapUpdate()
            )
          )
        )
      )
    case _ =>
      println("Unsupported JSON call: " + in)
  }

  def mapUpdate(): JsCmd =
    if (position.isDefined && position.is.isDefined)
      googleMapsClient.getCanvasCall(position.is.get, "map_canvas")
    else
      Noop

  private val waiting = <span>Waiting ...</span>

  def render = "#geo *" #> getGeoScript &
    "#longitude *" #> getLongitude &
    "#latitude *" #> getLatitude &
    "#altitude *" #> getAltitude &
    "#stops *" #> waiting &
    "#map *" #> getMap &
    "#googlemaps-init *" #> googleMapsClient.getGoogleAPIScript &
    "#googlemaps-create *" #> googleMapsClient.getStartScript &
    "#walking" #> SHtml.ajaxText(walkingDistance.meters.toString, setWalkingDistance(_)) &
    "#route" #> SHtml.ajaxText(route.get match {
      case Empty => ""
      case Full(route) => route.toString
    }, setRoute(_)) &
    "#stopcount" #> SHtml.ajaxText(stopCount.is.toString, setStopCount(_))

  private def setRoute(value: String): JsCmd = {
    try {
      route.set(Full(value.toInt))
    } catch {
      case e =>
        route.set(Empty)
    }
    CmdPair(
      SetHtml("stops", computeStops()),
      mapUpdate()
    )
  }

  private def setStopCount(value: String): JsCmd = {
    try {
      stopCount.set(value.toInt)
    } catch {
      case e =>
    }
    CmdPair(
      SetHtml("stops", computeStops()),
      mapUpdate()
    )
  }

  private def setWalkingDistance(dist: String): JsCmd = {
    try {
      walkingDistance.set(WalkingDistance(dist.toInt))
    } catch {
      case e => S.notice("Bad distance")
    }
    CmdPair(
      SetHtml("stops", computeStops()),
      mapUpdate()
    )
  }

  private def setPosition(map: Map[String, _]) {
    position.set(Full(Position(
      getArg(map, "latitude").get,
      getArg(map, "longitude").get,
      getArg(map, "altitude"))))
  }

  private def getArg(map: Map[String, _], key: String) =
    map.get(key) match {
      case Some(value: Double) => Some(value)
      case _ => None
    }

  private def getGeoScript = {
    <span>
      {Script(jsonInCode)}
      {Script(JsIf(JsVar("navigator.geolocation"),
      Call(
        "navigator.geolocation.getCurrentPosition",
        AnonFunc("coords", jsonCall("updatePosition", JsRaw("coords"))))))
      }
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

  private def computeStops() =
    position.is match {
      case Empty => waiting
      case Full(pos: Position) =>
        client.getStops(pos, walkingDistance, stopCount, route) match {
          case Stops(_, Nil, _) => waiting
          case Stops(_, stops, _) =>
            <ul>
              { stops.map (stop =>
              <li>
                { stop.Name }
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
          case _ => waiting
        }
    }
}
