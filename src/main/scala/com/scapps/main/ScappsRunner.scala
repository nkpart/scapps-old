package com.scapps.main

import org.eclipse.jetty.server.bio.SocketConnector
import org.eclipse.jetty.server.handler.ContextHandler
import org.eclipse.jetty.server.{Connector, Handler, Server}
import org.eclipse.jetty.servlet.{ServletHolder, ServletContextHandler}

object ScappsRunner {
  def main(args: Array[String]) {
    val server: Server = new Server(8081)
    val handler: ServletContextHandler = new ServletContextHandler(server, "/", ServletContextHandler.SESSIONS |
            ServletContextHandler.NO_SECURITY)

    val holder = new ServletHolder(classOf[slinky.http.servlet.StreamStreamServlet])
    holder.setInitParameter("application", "com.scapps.ScappsApplication")
    holder.setInitParameter("scapps-application", "com.scapps.MyApps")

    handler.addServlet(holder, "/*")
    server.setHandler(handler);

    server.start();
    server.join();
  }
}