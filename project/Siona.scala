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
    scalaVersion        := "2.9.1",
    scalacOptions       := Seq("-unchecked", "-deprecation", "-Ydependent-method-types"),
    crossScalaVersions  := Seq("2.9.1", "2.9.1-1", "2.9.2"),
    resolvers ++= Seq(
      "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
      "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
    )
  )
}

object Dependencies {
  val testDependencies = Seq(libraryDependencies += "org.specs2" %% "specs2" % "1.8.2" % "test")
}

object SionaBuild extends Build {
  import Dependencies._
  import BuildSettings._

  lazy val sindi = Project (
    "siona",
    file ("."),
    settings = buildSettings
  ) aggregate (core, logging)

  lazy val core = Project(
    "siona-core",
    file("siona-core"),
    settings = buildSettings ++ testDependencies ++ Seq(
      libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT",
        "com.chuusai" %% "shapeless" % "1.2.0-SNAPSHOT"
      )
    )
  )

  lazy val logging = Project(
    "siona-logging",
    file("siona-logging"),
    settings = buildSettings ++ testDependencies
  ) dependsOn(core)

  lazy val demo_petstore = Project(
    "siona-demo-petstore",
    file("siona-demo/petstore"),
    settings = buildSettings ++ testDependencies
  ) dependsOn(core, logging)
}

object Sonatype extends PublishToSonatype(SionaBuild) {
  def projectUrl    = "https://github.com/aloiscochard/siona"
  def developerId   = "alois.cochard"
  def developerName = "Alois Cochard"
  def licenseName   = "Apache 2 License"
  def licenseUrl    = "http://www.apache.org/licenses/LICENSE-2.0.html"
}
