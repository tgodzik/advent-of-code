package utils

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters._

object Inputs:
  extension (s: String)
    def input =
      val uri = getClass().getClassLoader().getResource(s).toURI()

      val path = Paths.get(uri)

      Files.readAllLines(path).asScala

