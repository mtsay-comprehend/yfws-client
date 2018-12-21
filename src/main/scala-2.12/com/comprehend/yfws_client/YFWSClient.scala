package com.comprehend.yfws_client

import com.hof.mi.web.service._
import com.typesafe.config.Config

class YFWSClient(service: AdministrationServiceInterface,
                 username: String,
                 password: String,
                 dbPassword: String) {

  def call(function: String)(filter: AdministrationServiceRequest => AdministrationServiceRequest): AdministrationServiceResponse = {
    val req = new AdministrationServiceRequest
    req.setLoginId(username)
    req.setPassword(password)
    req.setOrgId(1)
    req.setFunction(function)

    val resp = service.remoteAdministrationCall(filter(req))

    resp.getStatusCode match {
      case "SUCCESS" =>
        resp
      case statusCode =>
        val errorCode = resp.getErrorCode

        throw new IllegalStateException(s"Status Code: $statusCode, Error Code: $errorCode")
    }
  }

  def runQuery(query: String): Unit = {
    call("METADATASQLQUERY") { req =>
      req.setQuery(query)
      req.setParameters(Array(dbPassword))
      req
    }
  }

  def runExport(contentResource: ContentResource): AdministrationServiceResponse = {
    call("EXPORTCONTENT") { req =>
      req.setContentResources(Array(contentResource))
      req
    }
  }
}

/**
  * https://wiki.yellowfinbi.com/display/USER73Plus/Administration+Service
  */
object YFWSClient {
  def fromConfig(conf: Config): YFWSClient = {
    val host = conf.getString("host")
    val port = conf.getString("port").toInt
    val useSSL = conf.getString("useSSL").toBoolean
    val username = conf.getString("username")
    val password = conf.getString("password")
    // dbpassword is required only for METADATASQLQUERY
    val dbPassword = conf.getString("db_password").orElse("")

    val service = new AdministrationServiceServiceLocator(
      host,
      port,
      "/services/AdministrationService",
      useSSL
    ).getAdministrationService

    new YFWSClient(service, username, password, dbPassword)
  }
}
