package com.comprehend.yfws_client

import com.hof.mi.web.service._
import com.typesafe.config.Config

class ReportServiceClient(service: ReportServiceInterface,
                                  username: String,
                                  password: String,
                                  ) {

  def call(mutator: ReportServiceRequest => ReportServiceRequest): ReportServiceResponse = {
    val request = new ReportServiceRequest
    request.setLoginId(username)
    request.setPassword(password)
    request.setOrgId(1)

    val response = service.remoteReportCall(mutator(request))

    response.getStatusCode match {
      case "SUCCESS" =>
        response
      case statusCode =>
        val errorCode = response.getErrorCode

        throw new IllegalStateException(s"Status Code: $statusCode, Error Code: $errorCode")
    }
  }

}

/**
  * https://wiki.yellowfinbi.com/display/USER73Plus/Administration+Service
  */
object ReportServiceClient {
  def fromConfig(conf: Config): ReportServiceClient = {
    val host = conf.getString("host")
    val port = conf.getString("port").toInt
    val useSSL = conf.getString("useSSL").toBoolean
    val username = conf.getString("username")
    val password = conf.getString("password")

    val service = new ReportServiceServiceLocator(
      host,
      port,
      "/services/ReportService",
      useSSL
    ).getReportService

    new ReportServiceClient(service, username, password)
  }
}



