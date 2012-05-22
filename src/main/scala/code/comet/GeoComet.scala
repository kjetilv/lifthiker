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
import net.liftweb.http.js.JE._
import org.joda.time._
import code.model._

class GeoComet extends CometActor {

  private object userState extends SessionVar[UserState](UserState())

  private val address = new InetSocketAddress(System getProperty "trafikantenapi", 80)

  private val googleMaps = new GoogleMapsClient(System getProperty "googleapikey")

  private val trafikanten = new TrafikantenClient(address)

  private def commands(cmds: JsCmd*) = listCommands(cmds.toList)
  
  private def listCommands(cmds: List[JsCmd]): JsCmd = cmds match {
    case Nil => Noop
    case cmd :: Nil => cmd
    case cmd :: tail => CmdPair(cmd, listCommands(tail))
  }
  
  override def handleJson(in: Any): JsCmd = in match {
    case JsonCmd("selectStop", _, id: Double, _) =>
      SetHtml("realtime-canvas", getRealTimeData(id.toInt))
    case JsonCmd("positionUpdateFailed", _, map: Map[String, Map[String, _]], _) =>
      println("Oops: " + map)
    case JsonCmd("updatePosition", _, map: Map[String, Map[String, _]], _) =>
      val newPosition = readPosition(map("coords"))
      userState(userState at newPosition)
      commands(
        SetHtml("longitude", getLongitude),
        SetHtml("latitude", getLatitude),
        SetHtml("lastPositionUpdate", getLastPositionUpdate),
        updateMap()
      )
    case _ => println("Unsupported JSON call: " + in)
  }

  def updateMap() = commands(SetHtml("stops", computeStops()), mapUpdate()) 

  def mapUpdate(): JsCmd = userState.position match {
    case Some(pos) => googleMaps.zoomToCmd(pos, "map_canvas")
    case None => Noop
  }

  def render = "#geoscript *" #> getGeoScript &
    "#longitude *" #> getLongitude &
    "#latitude *" #> getLatitude &
    "#lastPositionUpdate *" #> getLastPositionUpdate &
    "#trackmap" #> setTrackMap() &
    "#trikk" #> setLimiter(userState.prefers(Trikk), (on: Boolean) => userState.withTransport(Trikk, on)) &
    "#bane" #> setLimiter(userState.prefers(Bane), (on: Boolean) => userState.withTransport(Bane, on)) &
    "#buss" #> setLimiter(userState.prefers(Buss), (on: Boolean) => userState.withTransport(Buss, on)) &
    "#baat" #> setLimiter(userState.prefers(Baat), (on: Boolean) => userState.withTransport(Baat, on)) &
    "#stops *" #> waiting &
    "#map *" #> getMap &
    "#googlemaps-init *" #> googleMaps.getGoogleAPIScript &
    "#walking" #> ajaxText(userState.range.map(_.meters), (v:String) => userState.withRange(WalkingDistance(v.toInt))) &
    "#route" #> ajaxText(userState.preferredRoute.map(_.id), (v: String) => userState.withPreferredRoute(TransportRoute(v.toInt))) &
    "#stopcount" #> ajaxText(userState.stopsToShow, (v: String) => userState.withStopsToShow(v.toInt))
  
  private def getLastPositionUpdate =
    <span> { userState.lastPositionUpdate.map(_ toString "HH:mm:ss").getOrElse("N/A") } </span>
  
  private val waiting = <span>Waiting ...</span>

  private def setLimiter(state: Boolean, newUserState: (Boolean) => UserState) = SHtml.ajaxCheckbox(state, 
    value => {
      userState(newUserState(value))
      updateMap()
    })
  
  private def ajaxText[T](state: Option[T], newUserState: String => UserState) = SHtml.ajaxText(state match {
    case Some(no) => no.toString
    case None => ""
  }, string => {
    userState(newUserState(string))
    updateMap()
  })
  
  private def setTrackMap() = SHtml.ajaxCheckbox(userState.trackPositionOnMap,
    value => { 
      userState(userState.withTrackedMap(value)) 
      if (value) updateMap() else Noop
    })

  def updatePosition(newPosition: Position) {
  }

  def readPosition(map: Map[String, _]): Position = Position(getArg(map, "latitude").get, getArg(map, "longitude").get)

  private def getArg(map: Map[String, _], key: String) =
    map.get(key) match {
      case Some(value: Double) => Some(value)
      case _ => None
    }

  private def getGeoScript = {
    <span>
      {Script(jsonInCode)}
      {Script(JsIf(JsVar("navigator.geolocation"),
      commands(
        Call(
          "navigator.geolocation.getCurrentPosition",
          AnonFunc("coords", jsonCall("updatePosition", JsRaw("coords"))),
          AnonFunc("error", jsonCall("positionUpdateFailed", JsRaw("error")))
        ),
        Call(
          "navigator.geolocation.watchPosition",
          AnonFunc("coords", jsonCall("updatePosition", JsRaw("coords")))))))
      }
    </span>
  }

  private def getMap = userState.position.map(googleMaps getURL _).map(mapIFrame(_)) getOrElse <span>Waiting...</span>

  private def mapIFrame(url: String) =
      <iframe width="750" height="350" frameborder="0" scrolling="no" marginheight="0" marginwidth="0" src={url}/>

  private def getLatitude = get(_.latitude)

  private def getLongitude= get(_.longitude)

  private def get[T](fun: Position => T) = 
    <span>{ userState.position.map(fun).getOrElse("Waiting...") }</span>

  private def getRealTimeData(stopId: Int) =
    <span>
      {
      realTimeByLines(stopId).map(_ match {
        case ((lineRef, toWhere, platformName), rts) =>
          <span>Line { lineRef } to { toWhere } from platform { platformName }
            <ul>{
              rts.map(entry(_)).map(s => <li>{ s } </li>)
              }
            </ul>
          </span>
      })
      }
    </span>

  private def realTimeByLines(stopId: Int) =
    trafikanten.getRealTime(stopId, userState).groupBy(rt =>
      (rt.LineRef, rt.DestinationName, rt.DeparturePlatformName)).toList.sortBy(_ match {
      case ((lineRef, _, _), _) => lineRef.toInt
    })

  private def printed(interval: Interval): String = printed(interval.toDuration)

  private def printed(duration: Duration): String =
    duration.getStandardMinutes + ":" + {
      val secs = duration.getStandardSeconds % 60
      if (secs < 10) "0" + secs else secs.toString
    }

  private def entry(rt: RealTime): String = {
    val now = new DateTime()
    val arrivalTime = new DateTime(rt.ExpectedArrivalTime)
    val duration = if (now isBefore arrivalTime) new Interval(now, arrivalTime).toDuration else Duration.ZERO
    "Vehicle " + rt.VehicleRef + " in block " + rt.BlockRef +
      (if (duration.isLongerThan(Duration.ZERO))
        if (duration.isLongerThan(Duration.standardMinutes(10)))
          " at " + arrivalTime.toString("HH:mm")
        else " in " + printed(duration)
      else "now") +
      (if (rt.ExpectedArrivalTime.getTime < rt.AimedArrivalTime.getTime)
        ", ahead of schedule: " + printed (new Interval(arrivalTime, new DateTime(rt.AimedArrivalTime)))
      else if (rt.ExpectedArrivalTime.getTime > rt.AimedArrivalTime.getTime)
        ", delayed: " + printed (new Interval(new DateTime(rt.AimedArrivalTime), arrivalTime))
      else " on time")
  }

  private def computeStops() =
    userState.position match {
      case Some(pos: Position) =>
        trafikanten.getStops(userState) match {
          case Nil => waiting
          case stops =>
            <span>
              {Script(jsonInCode)}
              <ul>
                { stops.map (stop =>
                <li>
                  { stop.Name }
                  { SHtml.ajaxButton("GOTO",
                  Call("selectStop", JsRaw(stop.position.latitude.toString),
                    JsRaw(stop.position.longitude.toString),
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
