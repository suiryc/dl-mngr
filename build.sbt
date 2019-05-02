import sbt._
import Keys._

lazy val versions = Map[String, String](
  "akka"                  → "2.5.21",
  "bouncycastle"          → "1.61",
  "config"                → "1.3.3",
  "dl-mngr"               → "0.0.1-SNAPSHOT",
  "httpclient"            → "4.5.7",
  "httpasyncclient"       → "4.1.4",
  "javafx"                → "12.0.1",
  "logback"               → "1.2.3",
  "monix"                 → "3.0.0-RC2",
  "netty"                 → "4.1.34.Final",
  "scala"                 → "2.12.8",
  "scala-logging"         → "3.9.2",
  "scalatest"             → "3.0.5",
  "scopt"                 → "3.7.1",
  "slf4j"                 → "1.7.26",
  "spray-json"            → "1.3.5",
  "suiryc-scala"          → "0.0.4-SNAPSHOT"
)


lazy val dlMngr = project.in(file(".")).
  enablePlugins(BuildInfoPlugin, GitVersioning).
  settings(
    organization := "suiryc",
    name := "dl-mngr",
    // Note: if we want to let sbt-git generate the version, we need to comment
    // "version", uncomment "git.baseVersion" and remove "-SNAPSHOT" (sbt-git
    // will append it if necessary).
    version := versions("dl-mngr"),
    //git.baseVersion := versions("dl-mngr"),
    scalaVersion := versions("scala"),

    buildInfoKeys := Seq[BuildInfoKey](
      name,
      version,
      git.gitHeadCommit,
      scalaVersion,
      sbtVersion
    ),
    buildInfoPackage := "suiryc.dl.mngr",
    buildInfoObject := "Info",
    buildInfoUsePackageAsPath := true,

    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-inaccessible",
      "-Ywarn-infer-any",
      "-Ywarn-dead-code",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
      "-Ywarn-unused",
      "-Ywarn-unused-import"
    ),
    resolvers += Resolver.mavenLocal,

    libraryDependencies ++= Seq(
      "ch.qos.logback"                 %  "logback-classic"               % versions("logback"),
      "com.github.scopt"               %% "scopt"                         % versions("scopt"),
      "com.typesafe"                   %  "config"                        % versions("config"),
      "com.typesafe.akka"              %% "akka-actor"                    % versions("akka"),
      "com.typesafe.akka"              %% "akka-slf4j"                    % versions("akka"),
      "com.typesafe.scala-logging"     %% "scala-logging"                 % versions("scala-logging"),
      "io.monix"                       %% "monix"                         % versions("monix"),
      "io.netty"                       %  "netty-all"                     % versions("netty"),
      "io.spray"                       %% "spray-json"                    % versions("spray-json"),
      "org.apache.httpcomponents"      %  "httpclient"                    % versions("httpclient")
        exclude ("commons-logging", "commons-logging"),
      "org.apache.httpcomponents"      %  "httpasyncclient"               % versions("httpasyncclient")
        exclude ("commons-logging", "commons-logging"),
      "org.bouncycastle"               %  "bcprov-jdk15on"                % versions("bouncycastle"),
      "org.openjfx"                    %  "javafx-base"                   % versions("javafx") classifier jfxPlatform,
      "org.openjfx"                    %  "javafx-controls"               % versions("javafx") classifier jfxPlatform,
      "org.openjfx"                    %  "javafx-fxml"                   % versions("javafx") classifier jfxPlatform,
      "org.openjfx"                    %  "javafx-graphics"               % versions("javafx") classifier jfxPlatform,
      "org.scalatest"                  %% "scalatest"                     % versions("scalatest")    % "test",
      "org.slf4j"                      %  "jcl-over-slf4j"                % versions("slf4j"),
      "suiryc"                         %% "suiryc-scala-core"             % versions("suiryc-scala"),
      "suiryc"                         %% "suiryc-scala-javafx"           % versions("suiryc-scala")
    ),

    publishMavenStyle := true,
    publishTo := Some(Resolver.mavenLocal)
  )

assemblyMergeStrategy in assembly := {
  case "module-info.class" => MergeStrategy.discard
  case x if x.startsWith("application.conf") ⇒ MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

lazy val jfxPlatform = {
  val osName = System.getProperty("os.name", "").toLowerCase
  if (osName.startsWith("mac")) "mac"
  else if (osName.startsWith("win")) "win"
  else "linux"
}

lazy val install = taskKey[Unit]("Installs application")
install := {
  import suiryc.scala.io.RichFile
  import suiryc.scala.sys.OS

  val projectFolder = baseDirectory.value
  val jar = assembly.value
  val targetFolder = if (OS.isLinux) {
    Path(RichFile.userHome) / "progs" / "dl-mngr"
  } else {
    Path("C:\\") / "Progs" / "dl-mngr"
  }
  sLog.value.info(s"Copying files to: $targetFolder")
  List(jar, projectFolder / "src" / "main" / "scripts" / "dl-mngr.py").foreach { src ⇒
    val targetPath = targetFolder / src.getName
    IO.copyFile(src, targetPath.asFile)
  }

  val projectResources = projectFolder / "src" / "main" / "resources"
  if (OS.isLinux) {
    // Install .desktop and icon files
    val userShare = Path(RichFile.userHome) / ".local" / "share"
    IO.copyFile(
      projectResources / "dl-mngr.desktop",
      userShare / "applications" / "dl-mngr.desktop"
    )
    IO.copyFile(
      projectResources / "dl-mngr.svg",
      userShare / "icons" / "dl-mngr.svg"
    )
  } else {
    IO.copyFile(
      projectResources / "dl-mngr.png",
      targetFolder / "dl-mngr.png"
    )
  }
}
