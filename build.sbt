name := "ddl-conc"

organization := "com.virdis"

version := "0.0.1"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.apache.opennlp" % "opennlp-tools" % "1.6.0"
)

initialCommands := "import com.virdis.ddlconc._"

