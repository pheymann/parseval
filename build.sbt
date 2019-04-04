
val `compiler-2.12` = Seq(
  "-deprecation",
  "-encoding", "utf-8",
  "-explaintypes",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:missing-interpolator",
  "-Xlint:option-implicit",
  "-Xlint:type-parameter-shadow",
  "-Xlint:unsound-match",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused:imports",
  "-Ywarn-unused:locals",
  "-Ywarn-unused:privates"
)

val common = Seq(
  organization  := "com.github.pheymann",
  scalaVersion  := "2.12.7",
  scalacOptions ++= `compiler-2.12`
)

lazy val root = project
  .in(file("."))
  .aggregate(parser, json)

lazy val parser = project
  .in(file("parser"))
  .settings(
    common,
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "4.5.1" % "test"
    )
  )

lazy val json = project
  .in(file("json"))
  .settings(
    common
  )
  .dependsOn(parser)
