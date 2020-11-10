name := "DuckJump"
version := "0.1"
scalaVersion := "2.13.3"

enablePlugins(ScalaJSPlugin)

// This is an application with a main methodreload
scalaJSUseMainModuleInitializer := true
mainClass in Compile := Some("duckjump.Main") // start with the `main` in Main

// ScalaTags -- depends on scalajs-dom
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.9.2"

// Akka Actors
libraryDependencies += "org.akka-js" %%% "akkajsactor" % "2.2.6.9"