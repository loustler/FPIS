val groupOfScalaLang = SettingKey[String]("groupOfScalaLang")
val prefixScala = SettingKey[String]("prefixScala")
val scalaTestVersion = SettingKey[String]("scalaTestVersion")

name := "fpis"

version := "1.0"

scalaVersion := "2.12.3"

prefixScala := "scala"
groupOfScalaLang := s"org.${prefixScala.value}-lang"

autoScalaLibrary := true

scalaTestVersion := "3.0.1"

libraryDependencies ++= Seq(
  groupOfScalaLang.value % s"${prefixScala.value}-library" % scalaVersion.value,
  groupOfScalaLang.value % s"${prefixScala.value}-reflect" % scalaVersion.value,
  groupOfScalaLang.value % s"${prefixScala.value}-compiler" % scalaVersion.value,
  "org.scalatest" %% "scalatest" % scalaTestVersion.value % "test",
  "org.scalactic" %% "scalactic" % scalaTestVersion.value
)
