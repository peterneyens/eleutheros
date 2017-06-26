import sbt._

object Dependencies {
  lazy val scalatest     = "org.scalatest" %% "scalatest" % "3.0.1"
  lazy val cats          = "org.typelevel" %% "cats" % "0.9.0"
  lazy val freestyle     = "io.frees" %% "freestyle" % "0.3.0"
  lazy val iota          = "com.47deg" %% "iota-core" % "0.2.1-SNAPSHOT"
  lazy val kindProjector = "org.spire-math" % "kind-projector" % "0.9.4"
}
