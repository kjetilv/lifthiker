/*
 * Copyright 2012 Kjetil Valstadsve
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package code.comet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.JsonCmd
import net.liftweb.http.js.JsCmds._
import java.net.InetSocketAddress
import net.liftweb.http.{SessionVar, SHtml, CometActor}
import net.liftweb.common.{Full, Empty, Box}
import net.liftweb.http.js.JE._
import code.model.{RealTime, Stops, WalkingDistance, Position}

class GeoComet extends CometActor {

  private object route extends SessionVar[Option[Int]](Some(37))

  private object position extends SessionVar[Option[Position]](None)

  private object walkingDistance extends SessionVar[Option[Int]](Some(500))

  private object stopCount extends SessionVar[Option[Int]](Some(5))

  private object trip extends SessionVar[Option[Int]](None)

  private val address = new InetSocketAddress(System getProperty "trafikantenapi", 80)

  private val googleMapsClient = new GoogleMapsClient(System getProperty "googleapikey")

  private val client = new TrafikantenClient(address)

  override def handleJson(in: Any): JsCmd = in match {
    case JsonCmd("selectStop", _, id: Double, _) =>
      SetHtml("realtime-canvas", getRealTimeData(id.toInt, route))
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
    case _ => println("Unsupported JSON call: " + in)
  }

  def mapUpdate(): JsCmd =
    if (position.isDefined && position.is.isDefined) googleMapsClient.getCanvasCall(position.is.get, "map_canvas")
    else Noop

  private val waiting = <span>Waiting ...</span>

  def render = "#geo *" #> getGeoScript &
    "#longitude *" #> getLongitude &
    "#latitude *" #> getLatitude &
    "#altitude *" #> getAltitude &
    "#stops *" #> waiting &
    "#map *" #> getMap &
    "#googlemaps-init *" #> googleMapsClient.getGoogleAPIScript &
    "#googlemaps-create *" #> googleMapsClient.getStartScript &
    "#walking" #> ajaxSet(walkingDistance) &
    "#route" #> ajaxSet(route) &
    "#stopcount" #> ajaxSet(stopCount) &
    "#trip" #> ajaxSet(trip)

  private def ajaxSet(sv: SessionVar[Option[Int]]) = SHtml.ajaxText(sv.is match {
    case Some(no) => no.toString
    case None => ""
  }, string => {
    try { 
      sv.set(Full(string.toInt))
    } catch {
      case e => sv.set(Empty)
    }
    CmdPair(
      SetHtml("stops", computeStops()),
      mapUpdate()
    )    
  })

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
    position.map(p => <iframe width="750" height="350"
                              frameborder="0" scrolling="no" marginheight="0" marginwidth="0"
                              src={ googleMapsClient getURL p }/>).getOrElse(<span>Waiting...</span>)

  private def getLatitude = get(_.latitude)

  private def getLongitude= get(_.longitude)

  private def getAltitude = get(_.altitude)

  private def get[T](fun: Position => T) = <span>{position.map(fun).getOrElse("Waiting...")}</span>

  def getRealTimeData(stopId: Int, route: Option[Int]) = {
    val realTimeMap = 
      client.retreiveRealTime(stopId, route) groupBy (rt => (rt.DestinationName, rt.DeparturePlatformName))
    <span>
      {
        realTimeMap map (_ match {
          case ((toWhere, platformName), rts) =>
            <span>To { toWhere } from platform { platformName }
              <ul>{
                rts.groupBy(rt => rt.LineRef) map (_ match {
                  case (routeId, lineRts) => 
                    <li>Line: {routeId} Arrivals: <ul> { 
                      lineRts.map(lineRt => <li>{ lineRt.ExpectedArrivalTime} </li>)
                      }
                    </ul>
                    </li>
                })
                }
              </ul>
            </span>
        })
      }
    </span>
  }
  
  private def computeStops() =
    position.is match {
      case Some(pos: Position) =>
        client.getStops(pos, walkingDistance.is map (WalkingDistance(_)), stopCount, route, trip) match {
          case Nil => waiting
          case stops =>
            <span>
              {Script(jsonInCode)}
              <ul>
                { stops.map (stop =>
                <li>
                  { stop.Name }
                  { SHtml.ajaxButton("GOTO", 
                  Call("selectStop", 
                    JsRaw(stop.latitude.toString), 
                    JsRaw(stop.longitude.toString), 
                    JsRaw(stop.ID.toString)), 
                  () => jsonCall("selectStop", JsRaw(stop.ID.toString))) }
                  <ul>
                    { stop.Lines.map(line => { <li>{
                    <span>{ line.mode}</span>
                      <span>&nbsp;</span>
                      <span>{ line.LineName }</span>
                    }</li>})
                    }
                  </ul>
                </li>) }
              </ul>
            </span>
        }
      case None => waiting
    }
}
