package com.comprehend.yfws_client

import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.nio.file.Files
import java.util.Base64

import com.hof.mi.web.service._
import com.typesafe.config.ConfigFactory

import scala.language.postfixOps

/**
  * See more function names at https://wiki.yellowfinbi.com/display/USER73Plus/Administration+Service
  */
object Main {

  val configname = "application-local"
  val adminClient = AdministrationServiceClient.fromConfig(ConfigFactory.load(configname))
  val reportClient = ReportServiceClient.fromConfig(ConfigFactory.load(configname))

  def main(args: Array[String]): Unit = {
    getChartsAndSafeCharts

    //listClients

    //listGroups

    // this will print available reports/views/....
    //getContent

    // this will export report metadata, this is not a content
    //val contentResource = getContent("Subject Visit - Listing")
    //exportContent(contentResource, "output.xml")

    //moreCompareContent
  }

  def getChartsAndSafeCharts: Unit = {
    val response = reportClient.call { request =>
      request.setReportRequest("HTMLCHARTONLY")
      request.setReportId(76251);
      request.setObjectName("Subject Screening Summary");
      request
    }

    println("Chart Size:" + response.getCharts.size)
    printCharts(response)
    saveCharts(response, "target/chart-from-yf")
  }

  def saveCharts(response: ReportServiceResponse, filename: String): Unit = {
    response.getCharts foreach  { chart =>
      val decoded = Base64.getMimeDecoder.decode(chart.getData)
      val bos = new BufferedOutputStream(new FileOutputStream(filename+System.currentTimeMillis+".png"))
      bos.write(decoded)
      bos.close()
    }
  }

  def printCharts(response: ReportServiceResponse): Unit = {
    print(
      s"""
         |ReportId: ${response.getReportId}
         |ReportUUID: ${response.getReportUUID}
         |ReportName: ${response.getReportName}
         |DrillCode: ${response.getDrillCode}
         |Chart.size: ${response.getCharts.size}
         |ReportStyle: ${response.getReportStyle.substring(0,100)}
         |PreRunFilterString: ${response.getPreRunFilterString}
        """.stripMargin)
  }

  def moreCompareContent: Unit = {
    val content2 = "0504.xml"

    val oldContent = adminClient.call("GETCONTENT") {
      identity
    } getContentResources

    val xmlContent = Array(getContentFromFile(content2))

    val newContent = adminClient.call("GETIMPORTCONTENT") { req =>
      req.setParameters(xmlContent)
      req
    } getContentResources

    val importOptions = newContent.zipWithIndex flatMap { case (newResource, index) =>
      oldContent.find(_.getResourceUUID == newResource.getResourceUUID) match {
        case Some(oldResource) if oldResource.getResourceType == "VIEW" =>
          Map(
            "SKIP" -> "False",
            "OPTION" -> "REPLACE",
            "EXISTING" -> oldResource.getResourceId.toString
          ) map {
            case(key, value) =>
              val importOption = new ImportOption
              importOption.setItemIndex(index)
              importOption.setOptionKey(key)
              importOption.setOptionValue(value)
              importOption
          }
        case Some(_) =>
          Map(
            "SKIP" -> "True"
          ) map {
            case(key, value) =>
              val importOption = new ImportOption
              importOption.setItemIndex(index)
              importOption.setOptionKey(key)
              importOption.setOptionValue(value)
              importOption
          }
        case None =>
          Array.empty[ImportOption]
      }
    }

    val response = adminClient.call("IMPORTCONTENT") { req =>
      req.setParameters(xmlContent)
      req.setImportOptions(importOptions)
      req
    }
  }

  def importOldContent: Unit = {
    val content1 = "0428.xml"
    adminClient.call("IMPORTCONTENT") { req =>
      req.setParameters(Array(getContentFromFile(content1)))
      req
    }
  }

  def compareContent: Unit = {
    val content2 = "0504.xml"

    val oldContent = adminClient.call("GETCONTENT") {
      identity
    } getContentResources

    val newContent = adminClient.call("GETIMPORTCONTENT") { req =>
      req.setParameters(Array(getContentFromFile(content2)))
      req
    } getContentResources

    val uuids = (oldContent ++ newContent).map(_.getResourceUUID ?? "null").toList.distinct.sorted

    val fields: List[(String, ContentResource => Any)] = List(
      ("ID", _.getResourceId),
      ("Type", _.getResourceType),
      ("Name", _.getResourceName),
      ("Description", _.getResourceDescription),
      ("OrgId", _.getResourceOrgId),
      ("Code", _.getResourceCode)
    )

    uuids foreach { uuid =>
      List(oldContent, newContent).map(_.find(_.getResourceUUID == uuid)) match {
        case List(optOldResource, optNewResource) =>
          if (optOldResource.isEmpty) print("> ")
          else if (optNewResource.isEmpty) print("< ")
          println(s"$uuid:")

          fields foreach {
            case (name, getValue) =>
              val oldValue = optOldResource.map(getValue).flatMap(Option(_)).map(_.toString).getOrElse("")
              val newValue = optNewResource.map(getValue).flatMap(Option(_)).map(_.toString).getOrElse("")

              if (oldValue != newValue) println(s"  $name: $oldValue => $newValue")
          }
      }
    }
  }

  def printToCSV(resource: ContentResource): Unit = {
    val fields: List[ContentResource => Any] = List(
      _.getResourceUUID,
      _.getResourceId,
      _.getResourceType,
      _.getResourceName,
      _.getResourceDescription,
      _.getResourceOrgId,
      _.getResourceCode
    )

    println(fields map { _ apply resource } mkString ",")
  }

  def printContentResource(resource: ContentResource): Unit = {
    print(
      s"""
         |ID: ${resource.getResourceId}
         |UUID: ${resource.getResourceUUID}
         |Type: ${resource.getResourceType}
         |Name: ${resource.getResourceName}
         |Description: ${resource.getResourceDescription}
         |OrgId: ${resource.getResourceOrgId}
         |Code: ${resource.getResourceCode}
        """.stripMargin)
  }

  def getContentFromFile(filename: String): String = {
    new String(Base64.getEncoder.encode(Files.readAllBytes(new File(filename).toPath)))
  }

  def flushCachedFilterCache: Unit = {
    adminClient.call("FLUSHCACHEDFILTERCACHE") { req =>
      req.setParameters(Array())
      req
    }
  }

/**
    * this method suppose to be used with getContent(String)
    * @param contentResource
    * @param filename
    */
  def exportContent(contentResource: ContentResource , filename: String): Unit = {
    val response = adminClient.runExport(contentResource)

    val bos = new BufferedOutputStream(new FileOutputStream(filename))
    response.getBinaryAttachments foreach { binary =>
      bos.write(binary.getData)
    }
    bos.close()
  }

  def getContent(resourceName: String): ContentResource = {
    val response = adminClient.call("GETCONTENT") {
      identity
    }

    response.getContentResources.filter(_.getResourceName == resourceName).head
  }

  def getContent: Unit = {
    val response = adminClient.call("GETCONTENT") {
      identity
    }

    response.getContentResources foreach { resource =>
      print(
        s"""
           |ResourceName:  ${resource.getResourceName}
           |ResourceId:  ${resource.getResourceId}
           |resourceUUID:  ${resource.getResourceUUID}
           |resourceType: ${resource.getResourceType}
         """.stripMargin
      )
    }
  }

  def listClients: Unit = {
    val response = adminClient.call("LISTCLIENTS") {
      identity
    }

    response.getClients foreach { clientOrg =>
      print(
        s"""
           |ID: ${clientOrg.getClientId}
           |Name: ${clientOrg.getClientName}
           |Reference ID: ${clientOrg.getClientReferenceId}
        """.stripMargin
      )
    }
  }

  def listGroups(clientId: String): Unit = {
    val response = adminClient.call("LISTGROUPS") { req =>
      req.setOrgRef(clientId)
      req
    }

    response.getGroups foreach { group =>
      print(
        s"""
          |ID: ${group.getGroupId}
          |Name: ${group.getGroupName}
          |Description: ${group.getGroupDescription}
          |Internal Reference: ${group.getGroupInternalReference}
        """.stripMargin
      )
    }
  }

  def updateGroupName(groupId: String, groupName: String): Unit = {
    adminClient.runQuery(
      s"""
         |UPDATE accessgroup
         |SET shortdescription = '$groupName'
         |WHERE internalreferenceid = '$groupId'
      """.stripMargin)
  }

  implicit class NullCoalescer[T](val v: T) extends AnyVal {
    def ??(defaultValue: T): T = { Option(v).getOrElse(defaultValue) }
  }

}
