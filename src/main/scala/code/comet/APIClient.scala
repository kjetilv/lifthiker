package code.comet

import org.jboss.netty.channel._
import code.model.{WalkingDistance, Position}
import org.jboss.netty.bootstrap.ClientBootstrap
import socket.nio.NioClientSocketChannelFactory
import java.util.concurrent.Executors
import vkode.scala.utils.CascadingActions
import java.net.InetSocketAddress
import org.jboss.netty.handler.codec.http._
import io.Codec
import org.jboss.netty.buffer.ChannelBuffer

object APIClient extends CascadingActions {

  private val address = new InetSocketAddress("api-test.trafikanten.no", 80)
  
  import org.jboss.netty.channel.Channels._

  def getStops(position: Position, 
               walkingDistance: WalkingDistance = WalkingDistance(1000),
               hits: Int = 10): String = {
    val future = bootstrap connect address
    val doneFuture = future.awaitUninterruptibly()
    if (!future.isSuccess) {
      throw new APIException("Failed to get stops", future.getCause)
    }
    val channel = doneFuture.getChannel
    val request = new DefaultHttpRequest(HttpVersion.HTTP_1_1, 
                                         HttpMethod.GET,  
      getStopsPath(position, walkingDistance, hits)) withActions (
      _.setHeader(HttpHeaders.Names.HOST, address.getHostName),
      _.setHeader(HttpHeaders.Names.CONNECTION, HttpHeaders.Values.CLOSE))
    val responseFuture = channel write request
    channel.getCloseFuture.awaitUninterruptibly()
    responseFuture.awaitUninterruptibly()
    handler.getResponse(channel)
  }

  def getStopsPath(position: Position, walkingDistance: WalkingDistance, hits: Int): String = {
    "/Place/GetClosestStopsAdvancedByCoordinates/?coordinates=" +
      "(X=" + position.x +
      ",Y=" + position.y +
      ")&proposals=" + hits +
      "&walkingDistance=" + walkingDistance.meters
  }

  private val bootstrap = new ClientBootstrap(new NioClientSocketChannelFactory(
    Executors.newCachedThreadPool(),
    Executors.newCachedThreadPool())) withAction (_ setPipelineFactory (factory))
  
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
