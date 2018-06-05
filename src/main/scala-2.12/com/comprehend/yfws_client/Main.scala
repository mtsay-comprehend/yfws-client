package com.comprehend.yfws_client

import java.io.File
import java.nio.file.Files
import java.util.Base64

import com.hof.mi.web.service.{ContentResource, ImportOption}
import com.typesafe.config.ConfigFactory

import scala.language.postfixOps

object Main {
  implicit class NullCoalescer[T](val v: T) extends AnyVal {
    def ??(defaultValue: T): T = { Option(v).getOrElse(defaultValue) }
  }

  val client = YFWSClient.fromConfig(ConfigFactory.load())

  val content1 = "0428.xml"
  val content2 = "0504.xml"

  def main(args: Array[String]): Unit = {
    moreCompareContent
  }

  def moreCompareContent: Unit = {
    val oldContent = client.call("GETCONTENT") {
      identity
    } getContentResources

    val xmlContent = Array(getContent(content2))

    val newContent = client.call("GETIMPORTCONTENT") { req =>
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

    val response = client.call("IMPORTCONTENT") { req =>
      req.setParameters(xmlContent)
      req.setImportOptions(importOptions)
      req
    }
  }

  def importOldContent: Unit = {
    client.call("IMPORTCONTENT") { req =>
      req.setParameters(Array(getContent(content1)))
      req
    }
  }

  def compareContent: Unit = {
    val oldContent = client.call("GETCONTENT") {
      identity
    } getContentResources

    val newContent = client.call("GETIMPORTCONTENT") { req =>
      req.setParameters(Array(getContent(content2)))
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

  def getContent(filename: String): String = {
    new String(Base64.getEncoder.encode(Files.readAllBytes(new File(filename).toPath)))
  }

  def flushCachedFilterCache: Unit = {
    client.call("FLUSHCACHEDFILTERCACHE") { req =>
      req.setParameters(Array())
      req
    }
  }

  def listClients: Unit = {
    val response = client.call("LISTCLIENTS") {
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
    val response = client.call("LISTGROUPS") { req =>
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
    client.runQuery(
      s"""
         |UPDATE accessgroup
         |SET shortdescription = '$groupName'
         |WHERE internalreferenceid = '$groupId'
      """.stripMargin)
  }
}
