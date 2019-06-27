import sbt._
import Keys._

lazy val versions = Map[String, String](
  "akka"                  -> "2.5.23",
  "bouncycastle"          -> "1.62",
  "config"                -> "1.3.4",
  "dl-mngr"               -> "0.0.1-SNAPSHOT",
  "httpclient"            -> "4.5.9",
  "httpasyncclient"       -> "4.1.4",
  "javafx"                -> "12.0.1",
  "logback"               -> "1.2.3",
  "monix"                 -> "3.0.0-RC3",
  "netty"                 -> "4.1.36.Final",
  "scala"                 -> "2.12.8",
  "scala-logging"         -> "3.9.2",
  "scalatest"             -> "3.0.8",
  "scopt"                 -> "3.7.1",
  "slf4j"                 -> "1.7.26",
  "spray-json"            -> "1.3.5",
  "suiryc-scala"          -> "0.0.4-SNAPSHOT"
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
      sbtVersion,
      BuildInfoKey.action("buildTime") {
        System.currentTimeMillis
      },
      libraryDependencies
    ),
    // Note: 'buildInfoOptions += BuildInfoOption.BuildTime' adds the UTC build time
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

    // Replace mappings for jar generation
    mappings in (Compile, packageBin) ~= remap,

    publishMavenStyle := true,
    publishTo := Some(Resolver.mavenLocal)
  )

def remap(mappings: Seq[(File, String)]): Seq[(File, String)] = {
  // Files to exclude: are generated inside 'target/classes' if running
  // from IDE. Useful when not cleaning up before packaging ...
  // Note: "application.conf*" is also discarded in assembly merge strategy.
  val exclude = Set("application.conf", "application.conf.bak", "state.json")
  // The 'package' path
  val matchPath = "package"
  // Get all files to package, and determine the actual destination path
  val toPackage = mappings.filter {
    case (_, dst) => (dst != matchPath) && Path(dst).asPath.startsWith(matchPath)
  }.map {
    case (src, dst) =>
      val dstPath = Path(dst).asPath
      src -> dstPath.getParent.resolveSibling(dstPath.getFileName).toString
  }
  val toPackageSrc = toPackage.map(_._1).toSet
  val toPackageDst = toPackage.map(_._2).toSet
  // Replace mappings that we are explicitly packaging
  mappings.filter {
    case (src, dst) => !toPackageSrc.contains(src) && !toPackageDst.contains(dst) && !exclude.contains(dst)
  } ++ toPackage
}

// Replace mappings for fat jar generation
assembledMappings in assembly ~= { mappings =>
  mappings.map { m =>
    if (m.sourcePackage.isEmpty) m.copy(mappings = remap(m.mappings).toVector)
    else m
  }
}

assemblyMergeStrategy in assembly := {
  case "module-info.class" => MergeStrategy.discard
  case x if x.startsWith("application.conf") => MergeStrategy.discard
  case x => (assemblyMergeStrategy in assembly).value.apply(x)
}

lazy val jfxPlatform = {
  val osName = System.getProperty("os.name", "").toLowerCase
  if (osName.startsWith("mac")) "mac"
  else if (osName.startsWith("win")) "win"
  else "linux"
}

lazy val installFolder = if (suiryc.scala.sys.OS.isLinux) {
  Path(suiryc.scala.io.RichFile.userHome) / "progs" / "dl-mngr"
} else {
  Path("C:\\") / "Progs" / "dl-mngr"
}

def installFiles(logger: Logger, projectFolder: File, files: Seq[(File, File)]): Unit = {
  import suiryc.scala.io.RichFile
  import suiryc.scala.sys.OS

  logger.info(s"Copying files to: $installFolder")
  val options = CopyOptions().withPreserveLastModified(true)
  IO.copy(files, options)
  // Remove previous (legacy) files
  val purgeFiles = Option((installFolder / "lib").listFiles).map(_.toSet).getOrElse(Set.empty) -- files.map(_._2)
  if (purgeFiles.nonEmpty) {
    logger.info(s"Cleaning legacy libs: ${purgeFiles.map(_.getName).toList.sorted.mkString(", ")}")
    IO.delete(purgeFiles)
  }

  List(projectFolder / "src" / "main" / "scripts" / "dl-mngr.py").foreach { src =>
    val targetPath = installFolder / src.getName
    IO.copyFile(src, targetPath.asFile, options)
  }

  val projectResources = projectFolder / "resources"
  if (OS.isLinux) {
    // Install .desktop and icon files
    val userShare = Path(RichFile.userHome) / ".local" / "share"
    IO.copyFile(
      projectResources / "dl-mngr.desktop",
      userShare / "applications" / "dl-mngr.desktop",
      options
    )
    IO.copyFile(
      projectResources / "dl-mngr.svg",
      userShare / "icons" / "dl-mngr.svg",
      options
    )
  } else {
    IO.copyFile(
      projectResources / "dl-mngr.png",
      installFolder / "dl-mngr.png",
      options
    )
  }
}

lazy val installAssembly = taskKey[Unit]("Installs application with included dependencies")
installAssembly := {
  installFiles(sLog.value, baseDirectory.value, Seq((assembly.value, installFolder / "dl-mngr.jar")))
}

lazy val installWithDeps = taskKey[Unit]("Installs application with separate dependencies")
installWithDeps := {
  // The artifact generated by the project
  val projectArtifact = packageBin.in(Runtime).value
  // The dependencies
  // See: https://www.scala-sbt.org/release/docs/Howto-Classpaths.html
  // There are at least two ways to get the dependencies.
  // 1. from the 'update' task directly
  val projectDependencies = update.value.select(configurationFilter("runtime")).toList
  // Note: the sbt doc example points to something like
  //  Classpaths.managedJars(Runtime, Set("jar"), update.value).map(_.data).toList
  // alas it does not include all dependencies ...
  // 2. from the classpath
  //val projectDependencies = dependencyClasspath.in(Runtime).value.files.filter(_.isFile).toList
  val sources = (projectArtifact, installFolder / "dl-mngr.jar") :: projectDependencies.map { file =>
    (file, installFolder / "lib" / file.getName)
  }
  installFiles(sLog.value, baseDirectory.value, sources)
}

lazy val install = taskKey[Unit]("Installs applications")
install := { installWithDeps.value }
