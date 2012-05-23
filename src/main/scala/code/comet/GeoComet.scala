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
import net.liftweb.http.{SHtml, CometActor}
import net.liftweb.http.js.JE._
import org.joda.time._
import code.model._
import java.text.DecimalFormat

class GeoComet extends CometActor {

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
      UserState.update(_ at readPosition(map("coords")))
      commands(
        SetHtml("longitude", getLongitude),
        SetHtml("latitude", getLatitude),
        SetHtml("lastPositionUpdate", getLastPositionUpdate),
        updateStopsCommands()
      )
    case _ => println("Unsupported JSON call: " + in)
  }

  def updateStopsCommands() = commands(SetHtml("stops", computeStops()), moveAroundInMap())

  def render = "#geoscript *" #> getGeoScript &
    "#longitude *" #>
      getLongitude &
    "#latitude *" #>
      getLatitude &
    "#recenter" #> 
      recenter() &
    "#lastPositionUpdate *" #>
      getLastPositionUpdate &
    "#trikk" #>
      setTypeLimiter(Trikk) &
    "#bane" #>
      setTypeLimiter(Bane) &
    "#buss" #>
      setTypeLimiter(Buss) &
    "#baat" #>
      setTypeLimiter(Baat) &
    "#stops *" #>
      waiting &
    "#map *" #>
      getMap &
    "#googlemaps-init *" #>
      googleMaps.getGoogleAPIScript &
    "#trackmap" #>
      toggleAttribute(_.trackPositionOnMap, _ withTrackedMap _) &
    "#walking " #>
      setAttribute[WalkingDistance](_.range, _.meters, s => WalkingDistance(s.toInt), _ withRange _) &
    "#route" #>
      setAttribute[TransportRoute](_.preferredRoute, _.id, s => TransportRoute(s.toInt), _ withPreferredRoute _) &
    "#stopcount" #>
      setAttribute[Int](_.stopsToShow, _.toString, _.toInt, _ withStopsToShow _)

  private def userState = UserState.get

  private def updateUserState(updater: UserState => UserState) = UserState update updater
  
  private def recenter() = SHtml.ajaxButton(
    "Recenter", 
    () => {
      updateUserState(_ withSelectedStop None)
      updateStopsCommands()
    })
  
  private def moveAroundInMap(): JsCmd = userState.mapPosition() match {
    case Some(pos) => googleMaps.zoomToCmd(pos, "map_canvas")
    case None => Noop
  }

  private def toggleAttribute(value: (UserState => Boolean),
                              updater: (UserState, Boolean) => UserState,
                              updateAlways: Boolean = false) =
    SHtml.ajaxCheckbox(value(userState),
      value => {
        updateUserState(updater(_, value))
        if (value || updateAlways) updateStopsCommands() else Noop
      })


  private def setAttribute[T](value: UserState => Option[T],
                              presenter: T => Any,
                              converter: (String => T),
                              updater: (UserState, Option[T]) => UserState) =
    SHtml.ajaxText(
      toString(value(userState), presenter),
      (string) => {
        updateUserState(updater(_, toValue(converter, string)))
        updateStopsCommands()
      })

  def toString[T](value: Option[T], present: T => Any) =
    value.map(present(_)).map(_.toString).getOrElse("")

  def toValue[T](convert: (String) => T, string: String): Option[T] = {
    try {
      Option(string).map(_.trim).filterNot(_.isEmpty).map(convert(_))
    } catch {
      case e =>
        e.printStackTrace(System.out)
        None
    }
  }

  private def setTypeLimiter(tt: TransportType) =
    limiterCheckBox(userState canRideWith tt, _.withTransport(tt, _))

  private def getLastPositionUpdate =
    <span> { userState.lastPositionUpdate.map(_ toString "HH:mm:ss").getOrElse("N/A") } </span>

  private val waiting = <span>Waiting ...</span>

  private def limiterCheckBox(state: Boolean, newUserState: (UserState, Boolean) => UserState) =
    SHtml.ajaxCheckbox(
      state,
      value => {
        UserState.update(newUserState(_, value))
        updateStopsCommands()
      })

  def readPosition(map: Map[String, _]): Option[Position] =
    try {
      Some(Position(getArg(map, "latitude").get, getArg(map, "longitude").get))
    } catch {
      case e =>
        e.printStackTrace()
        None
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

  private def getLatitude = getDouble(_.latitude)

  private def getLongitude= getDouble(_.longitude)

  private val nf = new DecimalFormat("00.000000")
  
  private def getDouble(fun: Position => Double) =
    <span>{ userState.position.map(fun).map(nf format _).getOrElse("0.0") }</span>

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
                  () => {
                    updateUserState(_ withSelectedStop Option(stop))
                    jsonCall("selectStop", JsRaw(stop.ID.toString))
                  })
                  }
                  <ul>
                    {
                    stop.Lines.groupBy(_.Transportation).toList.map(_ match {
                      case (transportation, lines) =>
                        <li>
                          { mode(transportation) }:
                          { lines.map(line => {
                          <span>&nbsp;</span>
                            <span>{ line.LineName }</span>
                        })
                          }
                        </li>
                    })
                    }
                  </ul>
                </li>) }
              </ul>
            </span>
        }
      case None => waiting
    }

  def mode(transportation: Int) = transportation match {
    case 8 => "Bane"
    case 7 => "Trikk"
    case 5 => "Båt"
    case 2 => "Buss"
    case x => x.toString
  }
}
