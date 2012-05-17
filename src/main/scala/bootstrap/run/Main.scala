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
