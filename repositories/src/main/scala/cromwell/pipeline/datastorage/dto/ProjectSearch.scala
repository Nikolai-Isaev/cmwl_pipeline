package cromwell.pipeline.datastorage.dto

import cromwell.pipeline.utils.json.AdtJsonFormatter
import play.api.libs.functional.syntax.{ toFunctionalBuilderOps, toInvariantFunctorOps, unlift }
import play.api.libs.json.Format.GenericFormat
import play.api.libs.json.{ Json, __, _ }

sealed trait ProjectSearchFilter
case object All extends ProjectSearchFilter
final case class ByName(mode: NameSearchMode, value: String) extends ProjectSearchFilter
final case class ByConfig(mode: ContentSearchMode, value: Boolean) extends ProjectSearchFilter
final case class ByFiles(mode: ContentSearchMode, value: Boolean) extends ProjectSearchFilter
final case class Or(left: ProjectSearchFilter, right: ProjectSearchFilter) extends ProjectSearchFilter
final case class And(left: ProjectSearchFilter, right: ProjectSearchFilter) extends ProjectSearchFilter

object ProjectSearchFilter {
  implicit val allFormat: OFormat[All.type] = AdtJsonFormatter.objectFormat(All)
  implicit val byNameFormat: OFormat[ByName] = Json.format
  implicit val byConfigFormat: OFormat[ByConfig] = Json.format
  implicit val byFilesFormat: OFormat[ByFiles] = Json.format

  implicit lazy val orReads: Reads[Or] =
    ((__ \ "left").lazyRead(projectSearchFilterReads) ~
      (__ \ "right").lazyRead(projectSearchFilterReads))(Or)
  implicit lazy val orWrites: OWrites[Or] =
    ((__ \ "left").lazyWrite(projectSearchFilterWrites) ~
      (__ \ "right").lazyWrite(projectSearchFilterWrites))(unlift(Or.unapply))

  implicit lazy val andReads: Reads[And] =
    ((__ \ "left").lazyRead(projectSearchFilterReads) ~
      (__ \ "right").lazyRead(projectSearchFilterReads))(And)
  implicit lazy val andWrites: OWrites[And] =
    ((__ \ "left").lazyWrite(projectSearchFilterWrites) ~
      (__ \ "right").lazyWrite(projectSearchFilterWrites))(unlift(And.unapply))

  implicit lazy val orFormat: OFormat[Or] = OFormat(orReads, orWrites)
  implicit lazy val andFormat: OFormat[And] = OFormat(andReads, andWrites)

  implicit lazy val projectSearchFilterReads: Reads[ProjectSearchFilter] =
    AdtJsonFormatter.adtReads("type")(
      AdtJsonFormatter.adtCase[All.type]("all"),
      AdtJsonFormatter.adtCase[ByName]("name"),
      AdtJsonFormatter.adtCase[ByFiles]("files"),
      AdtJsonFormatter.adtCase[ByConfig]("configurations"),
      AdtJsonFormatter.adtCase[Or]("or"),
      AdtJsonFormatter.adtCase[And]("and")
    )
  implicit lazy val projectSearchFilterWrites: OWrites[ProjectSearchFilter] =
    AdtJsonFormatter.adtWrites("type")(
      AdtJsonFormatter.adtCase[All.type]("all"),
      AdtJsonFormatter.adtCase[ByName]("name"),
      AdtJsonFormatter.adtCase[ByFiles]("files"),
      AdtJsonFormatter.adtCase[ByConfig]("configurations"),
      AdtJsonFormatter.adtCase[Or]("or"),
      AdtJsonFormatter.adtCase[And]("and")
    )
}

sealed trait NameSearchMode
case object FullMatch extends NameSearchMode
case object RegexpMatch extends NameSearchMode

object NameSearchMode {
  implicit lazy val nameSearchModeFormat: Format[NameSearchMode] = {
    implicitly[Format[String]].inmap(NameSearchMode.fromString, NameSearchMode.toString)
  }

  def fromString(s: String): NameSearchMode = s match {
    case "full_match"   => FullMatch
    case "regexp_match" => RegexpMatch
  }

  def toString(mode: NameSearchMode): String = mode match {
    case FullMatch   => "full_match"
    case RegexpMatch => "regexp_match"
  }
}

sealed trait ContentSearchMode
case object Exists extends ContentSearchMode

object ContentSearchMode {
  implicit lazy val contentSearchModeFormat: Format[ContentSearchMode] = {
    implicitly[Format[String]].inmap(ContentSearchMode.fromString, ContentSearchMode.toString)
  }

  def fromString(s: String): ContentSearchMode = s match {
    case "exists" => Exists
  }

  def toString(mode: ContentSearchMode): String = mode match {
    case Exists => "exists"
  }
}

final case class ProjectSearchRequest(filter: ProjectSearchFilter)
object ProjectSearchRequest {
  implicit lazy val projectSearchAdditionRequestFormat: OFormat[ProjectSearchRequest] =
    Json.format[ProjectSearchRequest]
}

final case class ProjectSearchResponse(data: Seq[Project])
object ProjectSearchResponse {
  implicit lazy val projectSearchResponseFormat: OFormat[ProjectSearchResponse] =
    Json.format[ProjectSearchResponse]
}
