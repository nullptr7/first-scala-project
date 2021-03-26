version in ThisBuild := "0.0.1"

scalaVersion in ThisBuild := "2.12.13"

mainClass in (Compile, run) := Some("TrampolineDemo") 
// set the main class for packaging the main jar
mainClass in (Compile, packageBin) := Some("TrampolineDemo")

// set the main class for the main 'sbt run' task

watchTriggeredMessage in ThisBuild := Watch.clearScreenOnTrigger

initialCommands in console := "import homegrown.collections._"

addCommandAlias("testc", ";clean;coverage;test;coverageReport")

scalacOptions ++= 
Seq (
    "-feature",
    "-language:implicitConversions"
)

libraryDependencies ++= Seq (
    "org.scalatest" %% "scalatest" % "3.2.3" % "test"
)