package suiryc.dl.mngr

import com.typesafe.scalalogging.StrictLogging
import io.netty.bootstrap.ServerBootstrap
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http.websocketx.{TextWebSocketFrame, WebSocketFrame, WebSocketServerProtocolHandler}
import io.netty.handler.codec.http.websocketx.extensions.compression.WebSocketServerCompressionHandler
import io.netty.handler.codec.http.{HttpObjectAggregator, HttpServerCodec}
import io.netty.handler.logging.LoggingHandler
import io.netty.util.concurrent.{Future => nettyFuture}
import spray.json._
import suiryc.dl.mngr.I18N.Strings
import suiryc.scala.sys.UniqueInstance

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit
import scala.concurrent.{CancellationException, Future, Promise}

/**
 * WebSocket server.
 *
 * Exposes an endpoint that can be used to pass commands as an alternative to
 * CLI.
 */
object WSServer extends StrictLogging {

  import Main.Akka.dispatcher

  private case class Instance(
    group: NioEventLoopGroup,
    port: Future[Int]
  )

  // Running instance if any.
  private var instance: Option[Instance] = None

  // Whether we are stopping.
  private var stopping = false

  /**
   * Starts the server.
   *
   * If the server is already started, simply returns the listening port of the
   * running instance.
   *
   * @return the listening port when the server is ready
   */
  def start(): Future[Int] = this.synchronized {
    instance match {
      case Some(inst) =>
        inst.port

      case None =>
        // Create a new instance.
        // See: https://netty.io/wiki/user-guide-for-4.x.html
        stopping = false
        val group = new NioEventLoopGroup(2)

        val b = new ServerBootstrap()
        b.group(group)
          .channel(classOf[NioServerSocketChannel])
          .handler(new LoggingHandler())
          .childHandler(new WebSocketServerInitializer())
        // Notes:
        // Depending on the browser, some ports actually cannot be used: trying
        // to connect to such ports results in an error (typically mentioning
        // security reasons).
        // See: https://stackoverflow.com/questions/4313403/why-do-browsers-block-some-ports
        // Letting the server use an available port (binding on '0') should be
        // fine because it usually chooses one from ephemeral ports (which are
        // not blocked).
        val bind = b.bind(0)

        val port = asScala(bind).map { channel =>
          // Belt and suspenders: if the channel is closed, make sure that
          // associated resources are released too.
          // It should only happen when we stop the server: we don't expect the
          // channel to close on its own.
          asScala(channel.closeFuture).onComplete { _ =>
            if (!stopping) logger.error("WebSocket channel was unexpectedly closed")
            stop()
          }
          channel.localAddress.asInstanceOf[InetSocketAddress].getPort
        }
        instance = Some(Instance(group, port))
        port
    }
  }

  /**
   * Stops the server.
   *
   * Does nothing if there is no running instance.
   *
   * @return a Future completed once the instance is stopped
   */
  def stop(): Future[_] = this.synchronized {
    instance match {
      case Some(inst) =>
        stopping = true
        instance = None
        asScala(inst.group.shutdownGracefully(0, Settings.SHUTDOWN_TIMEOUT.toSeconds, TimeUnit.SECONDS))

      case None =>
        Future.successful(())
    }
  }

  // Converts a netty Future into a scala Future.
  private def asScala[A](nettyFuture: nettyFuture[A]): Future[A] = {
    val promise = Promise[A]()
    nettyFuture.addListener((future: nettyFuture[A]) => {
      if (future.isSuccess) promise.success(future.getNow)
      else if (future.isCancelled) promise.failure(new CancellationException)
      else promise.failure(future.cause)
    })
    promise.future
  }

  // Converts a netty ChannelFuture into a scala Future.
  private def asScala(channelFuture: ChannelFuture): Future[Channel] = {
    val promise = Promise[Channel]()
    channelFuture.addListener((future: ChannelFuture) => {
      if (future.isSuccess) promise.success(future.channel)
      else if (future.isCancelled) promise.failure(new CancellationException)
      else promise.failure(future.cause)
    })
    promise.future
  }

  // WebSocket server pipeline handler.
  private class WebSocketServerInitializer extends ChannelInitializer[SocketChannel] {
    override def initChannel(ch: SocketChannel): Unit = {
      ch.pipeline
        .addLast(new HttpServerCodec())
        .addLast(new HttpObjectAggregator(64 * 1024))
        .addLast(new WebSocketServerCompressionHandler())
        .addLast(new WebSocketServerProtocolHandler("/", null, true))
        .addLast(new WebSocketFrameHandler())
      ()
    }
  }

  // WebSocket frame handler.
  //
  // JSON is used for incoming and outgoing frames.
  // Each incoming message is processed and the result is written back.
  // We at least handle the same parameters than on the CLI (class is shared).
  // The response is also similar to what we do on the CLI: result code and
  // optional output.
  // To handle multiple concurrent messages, an optional correlation id can be
  // set in the incoming message and is included in the corresponding response.
  private class WebSocketFrameHandler extends SimpleChannelInboundHandler[WebSocketFrame] {

    override protected def channelRead0(ctx: ChannelHandlerContext, frame: WebSocketFrame): Unit = {
      frame match {
        case frame: TextWebSocketFrame =>
          processMessage(frame.text).foreach { s =>
            ctx.channel.writeAndFlush(new TextWebSocketFrame(s))
          }

        case _ =>
          throw new UnsupportedOperationException(s"Unsupported frame type: ${frame.getClass.getName}")
      }
    }

    private def processMessage(msg: String): Future[String] = {
      import JsonProtocol._
      val paramsOpt = try {
        Right(msg.parseJson.convertTo[Main.Params])
      } catch {
        case ex: Exception => Left(ex)
      }
      val fExec = paramsOpt match {
        case Left(ex) =>
          Main.controller.displayError(
            title = None,
            contentText = Some(Strings.cliIssue),
            ex = ex
          )
          Future.successful(UniqueInstance.CommandResult(
            UniqueInstance.CODE_CMD_ERROR,
            Some(s"Failed to parse message: ${ex.getMessage}"))
          )

        case Right(params) =>
          Main.cmd(params)
      }
      fExec.recover {
        case ex: Exception =>
          Main.controller.displayError(
            title = None,
            contentText = Some(Strings.cliIssue),
            ex = ex
          )
          UniqueInstance.CommandResult(
            UniqueInstance.CODE_CMD_ERROR,
            Some(s"Failed to process arguments: ${ex.getMessage}")
          )
      }.map { r =>
        val result = CommandResult(
          correlationId = paramsOpt.toOption.flatMap(_.correlationId),
          code = r.code,
          output = r.output
        )
        result.toJson.compactPrint
      }
    }

  }

  private case class CommandResult(
    correlationId: Option[String] = None,
    code: Int,
    output: Option[String] = None
  )

  private object JsonProtocol extends DefaultJsonProtocol {
    implicit val paramsFormat: RootJsonFormat[Main.Params] = jsonFormat12(Main.Params)
    implicit val resultFormat: RootJsonFormat[CommandResult] = jsonFormat3(CommandResult)
  }

}
