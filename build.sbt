name := "Bank transaction cleanup"
version := "1.0"
scalaVersion := "2.13.8"

libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-xml" % "2.0.1",
                            "org.typelevel" %% "spire" % "0.17.0")

ThisBuild / scalafmtOnCompile := true
