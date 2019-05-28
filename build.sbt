name := "adt-scalajs-example"

version := "0.1"

scalaVersion := "2.12.8"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

scalacOptions ++= Seq(
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions"
)

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0" % "test"
//libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7"

jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()