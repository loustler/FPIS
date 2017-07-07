val groupOfScalaLang = SettingKey[String]("groupOfScalaLang")
val prefixScala = SettingKey[String]("prefixScala")

name := "fpis"

version := "1.0"

scalaVersion := "2.12.2"

prefixScala := "scala"
groupOfScalaLang := s"org.${prefixScala.value}-lang"

autoScalaLibrary := true

libraryDependencies ++= Seq(
  groupOfScalaLang.value % s"${prefixScala.value}-library" % scalaVersion.value,
  groupOfScalaLang.value % s"${prefixScala.value}-reflect" % scalaVersion.value,
  groupOfScalaLang.value % s"${prefixScala.value}-compiler" % scalaVersion.value,
  "org.scalatest" % "scalatest_2.12" % "3.0.1"
)