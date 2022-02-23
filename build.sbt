name := "Bank transaction cleanup"
version := "1.0"
scalaVersion := "2.13.8"

libraryDependencies ++= Seq("org.apache.poi" % "poi" % "5.0.0",
                            "org.apache.poi" % "poi-ooxml" % "5.0.0",
                            "org.scala-lang.modules" %% "scala-xml" % "2.0.1",
                            "org.typelevel" %% "spire" % "0.17.0")

ThisBuild / scalafmtOnCompile := true
