package bootstrap.run

import org.mortbay.jetty.Server
import org.mortbay.jetty.webapp.WebAppContext
import org.mortbay.jetty.nio._

object Main extends App {
  val server = new Server
  val scc = new SelectChannelConnector
  scc.setPort(8080)
  server.setConnectors(Array(scc))

  val context = new WebAppContext()
  context.setServer(server)
  context.setContextPath("/")
  context.setWar("src/main/webapp")

  server.addHandler(context)

  Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
    def run() {
      server.stop()
      server.join()
    }
  }, "ShutdownHook"))
  
  try {
    server.start()
  } catch {
    case exc: Exception => {
      exc.printStackTrace()
      System.exit(100)
    }
  }
}
