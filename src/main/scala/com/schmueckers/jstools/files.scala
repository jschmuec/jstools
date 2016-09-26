package com.schmueckers.jstools

import java.nio.file.FileSystems
import scala.collection.JavaConverters._
import java.nio.file.Path
import java.io.File

/** This package contains some useful functions to working with files.
  *
  * Unfortunately, there is no really good way to test this automatically.
  */
package object files {

  /** Implicit conversion so that the Path logic isn't
    * exposed to the outside code
    */
  implicit def toPath(filename: String) =
    FileSystems.getDefault.getPath(filename)

  /** Recursively goes through the directory and finds all files inside it
    * including directories
    */
  def find(dir: Path, includeHidden: Boolean = false): Iterator[Path] =
    java.nio.file.Files.walk(dir).iterator().asScala.filter(p => includeHidden || (!p.isHidden))

  def findRelative(dir: Path): Iterator[Path] =
    find(dir).map(dir.relativize(_)) // drop the actual file

  /** Recursively goes through the directory and finds all the regular files. So
    * directories and stuff will be excluded.
    */
  def findRegularFiles(dirName: String) = find(dirName).filter(_.isRegularFile)

  /** Adds a check to a Path that tells us if it is a regular file
    */
  implicit class PathImplicits(path: Path) {
    def isRegularFile = java.nio.file.Files.isRegularFile(path)
    def isHidden = java.nio.file.Files.isHidden(path)
  }

  /** Adds a predicate to check if a file is a regular file
    */
  implicit class FileImplicits(f: File) {
    def isRegularFile = f.toPath().isRegularFile
  }
}
