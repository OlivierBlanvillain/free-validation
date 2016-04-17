resolvers += "scalatl" at "http://milessabin.com/scalatl"

// scalaVersion := "2.11.8
scalaVersion := "2.11.8-tl-201604151108"

scalaBinaryVersion := "2.11"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.4.1",
  "com.github.mpilquist" %% "simulacrum" % "0.7.0",
  "io.github.jto" %% "validation-core" % "1.0.2",
  "io.github.jto" %% "validation-json" % "1.0.2",
  compilerPlugin("org.scalamacros" % "paradise_2.11.7" % "2.1.0-M5"),
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3"))

resolvers += Resolver.sonatypeRepo("snapshots")

scalacOptions := Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  // "-Xfatal-warnings",
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused-import",
  "-Xfuture"
)
