scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.4.1",
  "com.github.mpilquist" %% "simulacrum" % "0.7.0",
  "io.github.jto" %% "validation-core" % "1.1",
  "io.github.jto" %% "validation-json" % "1.1",
  compilerPlugin("org.scalamacros" % "paradise_2.11.8" % "2.1.0"),
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3"))

resolvers ++= Seq(Resolver.sonatypeRepo("snapshots"), Resolver.sonatypeRepo("public"))

scalacOptions := Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused-import",
  "-Xfuture")
