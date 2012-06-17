//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//

import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Sonatype.settings ++ Seq(
    organization        := "com.github.aloiscochard.siona",
    version             := "0.1-SNAPSHOT",
    scalaVersion        := "2.9.2",
    scalacOptions       := Seq("-unchecked", "-deprecation", "-Ydependent-method-types"),
    crossScalaVersions  := Seq("2.9.1", "2.9.1-1", "2.9.2"),
    resolvers ++= Seq(
      "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
      "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
      "eaio.com" at "http://eaio.com/maven2"
    )
  )
}

object Dependencies {
  val testDependencies = Seq(libraryDependencies <<= (scalaVersion, libraryDependencies) { (version, dependencies) =>
    val specs2 = version match {
      case "2.9.1" => ("org.specs2" %% "specs2" % "1.9" % "test")
      case "2.9.1-1" => ("org.specs2" %% "specs2" % "1.9" % "test")
      case _ => ("org.specs2" %% "specs2" % "1.11" % "test")
    }
    dependencies :+ specs2
  })
}

object SionaBuild extends Build {
  import Dependencies._
  import BuildSettings._

  lazy val sindi = Project (
    "siona",
    file ("."),
    settings = buildSettings
  ) aggregate (core, data, logging, demo_petstore)

  lazy val core = Project(
    "siona-core",
    file("siona-core"),
    settings = buildSettings ++ testDependencies ++ Seq(
      libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT",
        "org.scalaz" %% "scalaz-effect" % "7.0-SNAPSHOT",
        "com.eaio.uuid" % "uuid" % "3.3"
      )
    )
  )

  lazy val data = Project(
    "siona-data",
    file("siona-data"),
    settings = buildSettings ++ testDependencies ++ Seq(
      libraryDependencies ++= Seq(
        "com.fasterxml.jackson.core" % "jackson-core" % "2.0.0",
        //"com.chuusai" %% "shapeless" % "1.2.2"
        "com.github.aloiscochard" %% "shapeless" % "1.2.3-SNAPSHOT"
      )
    )
  ) dependsOn(core)

  lazy val logging = Project(
    "siona-logging",
    file("siona-logging"),
    settings = buildSettings ++ testDependencies
  ) dependsOn(core)

  lazy val demo_petstore = Project(
    "siona-demo-petstore",
    file("siona-demo/petstore"),
    settings = buildSettings ++ testDependencies
  ) dependsOn(core, data, logging)
}

object Sonatype extends PublishToSonatype(SionaBuild) {
  def projectUrl    = "https://github.com/aloiscochard/siona"
  def developerId   = "alois.cochard"
  def developerName = "Alois Cochard"
  def licenseName   = "Apache 2 License"
  def licenseUrl    = "http://www.apache.org/licenses/LICENSE-2.0.html"
}
