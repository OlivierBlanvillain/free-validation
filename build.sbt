
scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.spire-math" %% "cats" % "0.4.0-SNAPSHOT",
  "com.chuusai" %% "shapeless" % "2.2.5")

resolvers += Resolver.sonatypeRepo("snapshots")
