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
import vkode.scala.utils.CascadingActions
import java.net.InetSocketAddress
import org.jboss.netty.handler.codec.http._
import io.Codec
import org.jboss.netty.buffer.ChannelBuffer
import net.liftweb.json.JsonAST.{JNothing, JNull, JArray, JValue}
import net.liftweb.json.{DefaultFormats, JsonParser}
import net.liftweb.common.{Full, Box, Empty}
import code.model._

class TrafikantenClient(address: InetSocketAddress) extends CascadingActions {
  
  import org.jboss.netty.channel.Channels._

  private var stops: Option[Stops] = None 
  
  def getStops(position: Position, 
               walkingDistance: Box[WalkingDistance] = Full(WalkingDistance(1000)),
               hits: Box[Int] = Full(10),
               route: Box[Int] = Empty): Stops = {
    if (stops.isEmpty || longerDistance(walkingDistance) || farFrom(position)) {
      val stopList = retrieveStops(position, walkingDistance.map(_.times(10)), hits.map(_ * 10))
      stops = Some(Stops(position, stopList, walkingDistance))
    }
    stops.get.forRoute(route).scaledTo(hits, walkingDistance)
  }

  def farFrom(position: Position) = stops.get.farFrom(position)

  def longerDistance(walkingDistance: Box[WalkingDistance]) =
    walkingDistance.map(stops.get.walkingDistance.get.times(10).lessThan(_)).getOrElse(false)

  private def retrieveStops(position: Position, walkingDistance: Option[WalkingDistance], hits: Option[Int]): List[Stop] = {
    val future = bootstrap connect address
    val doneFuture = future.awaitUninterruptibly()
    if (!future.isSuccess) {
      throw new APIException("Failed to get stops", future.getCause)
    }
    val channel = doneFuture.getChannel
    val path = getStopsPath(position, walkingDistance, hits)
    val request = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, path) withActions(
      _.setHeader(HttpHeaders.Names.HOST, address.getHostName),
      _.setHeader(HttpHeaders.Names.CONNECTION, HttpHeaders.Values.CLOSE))
    val responseFuture = channel write request
    channel.getCloseFuture.awaitUninterruptibly()
    responseFuture.awaitUninterruptibly()
    val resultJson = handler.getResponse(channel)

    val stopList = JsonParser parseOpt resultJson match {
      case Some(array: JArray) => array.children.toList.map(stop(_)).filterNot(_.isEmpty).map(_.get)
      case Some(value: JValue) => stop(value).map(List(_)).getOrElse(Nil)
      case somethingElse => Nil
    }
    stopList.map(relevantStop(_)).filterNot(invalid(_)).sortBy(_.WalkingDistance)
  }
  
  private def relevantStop(stop: Stop): Stop = {
    val relevantLines = Option(stop.Lines).map(_.filter(_.LineID < 100))
    stop.copy(Lines = relevantLines.getOrElse(Nil))
  }
  
  private def invalid(stop: Stop) = 
    Option(stop.Lines) map (_.isEmpty) getOrElse true || !stop.Lines.contains((_:Line).LineID < 100)

  private implicit val fmtz = new DefaultFormats {
    override protected def dateFormatter = new java.text.SimpleDateFormat("EEE MMM dd HH:mm:ss Z yyyy")
  }
  
  private def stop(value: JValue) = value match {
    case JNull => None
    case JNothing => None
    case json => Some(json.extract[Stop])
  } 

  private def getStopsPath(position: Position, walkingDistance: Option[WalkingDistance], hits: Option[Int]): String = 
    "/Place/GetClosestStopsAdvancedByCoordinates/" +
      "?coordinates=(X=" + position.x + ",Y=" + position.y + ")" +
      hits.map("&proposals=" + _).getOrElse("") +
      walkingDistance.map("&walkingDistance=" + _.meters).getOrElse("")

  private val bootstrap = 
    new ClientBootstrap(new NioClientSocketChannelFactory(
      Executors.newCachedThreadPool(),
      Executors.newCachedThreadPool())
    ) withAction (_ setPipelineFactory (factory))
  
  object handler extends SimpleChannelUpstreamHandler {
    
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
      if (sb == null) 
        new StringBuilder withAction (channelSb.set(channel, _))
      else 
        sb
    }
  }    

  private object factory extends ChannelPipelineFactory {
    def getPipeline = pipeline() withActions ( 
      _.addLast("codes", new HttpClientCodec),
      _.addLast("gzip", new HttpContentDecompressor),
      _.addLast("handler", handler))
  }
}
