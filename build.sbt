name := "YFWS Client"

version := "1.0"

scalaVersion := "2.12.4"
    
libraryDependencies ++= Seq(
  "com.typesafe"      % "config"            % "1.3.2",
  "axis"              % "axis-wsdl4j"       % "1.2",
  "commons-discovery" % "commons-discovery" % "0.5",
  "javax.xml.rpc"     % "javax.xml.rpc-api" % "1.1",
  "org.apache.axis"   % "axis"              % "1.4"
)
