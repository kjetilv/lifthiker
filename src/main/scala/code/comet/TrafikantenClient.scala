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

import org.jboss.netty.channel._
import org.jboss.netty.bootstrap.ClientBootstrap
import socket.nio.NioClientSocketChannelFactory
import java.util.concurrent.Executors
import java.net.InetSocketAddress
import org.jboss.netty.handler.codec.http._
import io.Codec
import org.jboss.netty.buffer.ChannelBuffer
import net.liftweb.json.JsonAST.{JArray, JValue}
import code.model._
import java.util.Date
import net.liftweb.json._

class TrafikantenClient(address: InetSocketAddress) {

  import org.jboss.netty.channel.Channels._

  def getRealTime(userState: UserState) = 
    getRealTimePath(userState).map(read[RealTime](_)).map(_.filter(_.LineRef.toInt < 100)).map(list => {
      userState.preferredRoute match {
        case None => list
        case Some(routeId) => list.filter(_.LineRef == routeId.toString)
      }
    })
  
  def getTrip(id: Int): Trip = retrieveTrip(id)

  def getStops(userState: UserState): List[Stop] = {
    val stopList = retrieveStops(userState)
    val stops = Stops(userState.position.get, stopList, userState.range)
    stops.forRoute(userState).scaledTo(userState).stops
  }

  private def retrieveTrip(id: Int): Trip = read[Trip](getTripPath(id)) match {
    case singleTrip :: Nil => singleTrip
    case x => throw new APIException("Failed to parse trip: " + id)
  }

//  private def farFrom(userState: UserState) = 
//    if (stops.isDefined) (userState.position.map(stops.get farFrom _)).getOrElse(true) else true
//
//  private def longerDistance(userState: UserState) =
//    userState.range.map(stops.get.walkingDistance.get.times(10).lessThan(_)).getOrElse(false)
//
  private def retrieveStops(userState: UserState): List[Stop] = {
    val path = getStopsPath(userState)
    val stopList = read[Stop](path)
    stopList.map(relevantStop(_)).filterNot(invalid(_)).sortBy(_.WalkingDistance)
  }

  private def read[T](path: String)(implicit mf: Manifest[T]): List[T] = {
    val future = bootstrap connect address
    val doneFuture = future.awaitUninterruptibly()
    if (!future.isSuccess) throw new APIException("Failed to load path: " + path, future.getCause)

    val channel = doneFuture.getChannel
    val request = createRequest(path)
    val responseFuture = channel write request
    channel.getCloseFuture.await()
    responseFuture.await()

    val resultJson = handler getResponse channel
    val parsedJson = JsonParser parseOpt resultJson

    parsedJson match {
      case Some(array: JArray) => array.children.toList.map(_.extract[T])
      case Some(value: JValue) => value.extract[T] :: Nil
      case somethingElse =>
        throw new APIException("Could not make sense of: " + parsedJson)
    }
  }

  private def createRequest[T](path: String): HttpRequest = {
    val req = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, path)
    req.setHeader(HttpHeaders.Names.HOST, address.getHostName)
    req.setHeader(HttpHeaders.Names.CONNECTION, HttpHeaders.Values.CLOSE)
    req
  }

  private def relevantStop(stop: Stop): Stop = {
    val relevantLines = Option(stop.Lines).map(_.filter(_.LineID < 100))
    stop.copy(Lines = relevantLines.getOrElse(Nil))
  }

  private def invalid(stop: Stop) =
    Option(stop.Lines) map (_.isEmpty) getOrElse true || !stop.Lines.contains((_:Line).LineID < 100)

  private implicit val fmtz = new DefaultFormats {
    override val dateFormat = new DateFormat {
      def format(date: Date) =
        Conversions toArgument date
      def parse(string: String) =
        Some(string) map (Conversions fromValue _) map (_.getMillis) map (new Date(_))
    }
  }

  private def getRealTimePath(userState: UserState) = userState.selectedStop.map("/RealTime/GetRealTimeData/" + _.ID)

  private def getStopsPath(userState: UserState): String =
    userState.position.map(position => 
      "/Place/GetClosestStopsAdvancedByCoordinates/" +
        "?coordinates=(X=" + position.utmX + ",Y=" + position.utmY + ")" +
        userState.stopsToShow.map(_ * 10).map("&proposals=" + _).getOrElse("") +
        userState.range.map(_ times 10).map("&walkingDistance=" + _.meters).getOrElse("")).getOrElse(
      sys.error("No position: " + userState)
    )

  private def getTripPath(id: Int): String = "/Trip/GetTrip/" + id

  private val bootstrap = {
    val bs = new ClientBootstrap(new NioClientSocketChannelFactory(
      Executors.newCachedThreadPool(),
      Executors.newCachedThreadPool())
    )
    bs.setPipelineFactory(factory)
    bs
  }

  private object handler extends SimpleChannelUpstreamHandler {

    private val channelSb = new ChannelLocal[StringBuilder](false)

    override def messageReceived(ctx: ChannelHandlerContext, event: MessageEvent) {
      event.getMessage match {
        case response: { def getContent: ChannelBuffer } =>
          sb(ctx).append(response.getContent.toString(Codec.UTF8))
        case x =>
          println("Unexpected data: " + x)
      }
    }

    def getResponse(channel: Channel) =
      Option(channelSb remove channel) map (_.toString()) getOrElse ""

    private def sb(ctx: ChannelHandlerContext) = {
      val channel = ctx.getChannel
      val sb = channelSb.get(channel)
      if (sb == null) {
        val newSb = new StringBuilder()
        channelSb.set(channel, newSb)
        newSb
      } else sb
    }
  }

  private object factory extends ChannelPipelineFactory {
    def getPipeline = {
      val p = pipeline()
      p.addLast("codes", new HttpClientCodec)
      p.addLast("gzip", new HttpContentDecompressor)
      p.addLast("handler", handler)
      p
    }
  }
}
