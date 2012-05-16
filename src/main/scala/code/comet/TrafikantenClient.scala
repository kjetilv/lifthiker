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
import code.model.{Stops, Stop, WalkingDistance, Position}

class TrafikantenClient(address: InetSocketAddress) extends CascadingActions {
  
  import org.jboss.netty.channel.Channels._

  private var stops: Option[Stops] = None 
  
  def getStops(position: Position, 
               walkingDistance: WalkingDistance = WalkingDistance(1000),
               hits: Int = 10): List[Stop] = {
    if (stops.isEmpty || 
      stops.get.walkingDistance.times(10).lessThan(walkingDistance) || 
      stops.get.farFrom(position)) {
      val stopList = retrieveStops(position, walkingDistance.times(10), hits * 10)
      stops = Some(Stops(position, stopList, walkingDistance))
    }
    stops.get.scaledTo(hits, walkingDistance).stops
  }

  private def retrieveStops(position: Position, walkingDistance: WalkingDistance, hits: Int): List[Stop] = {
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
    stopList.sortBy(_.WalkingDistance)
  }

  private implicit val fmtz = new DefaultFormats {
    override protected def dateFormatter = new java.text.SimpleDateFormat("EEE MMM dd HH:mm:ss Z yyyy")
  }
  
  private def stop(value: JValue) = value match {
    case JNull => None
    case JNothing => None
    case json => Some(json.extract[Stop])
  } 

  private def getStopsPath(position: Position, walkingDistance: WalkingDistance, hits: Int): String = 
    "/Place/GetClosestStopsAdvancedByCoordinates/" +
      "?coordinates=(X=" + position.x + ",Y=" + position.y + ")" +
      "&proposals=" + hits +
      "&walkingDistance=" + walkingDistance.meters

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
