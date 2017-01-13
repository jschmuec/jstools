package com.schmueckers.jstools

import java.io.File
import java.nio.file.{FileSystems, Path}

import scala.language.implicitConversions

/**
  * This package contains some useful functions to working with files.
  *
  * Unfortunately, there is no really good way to test this automatically.
  */
package object files {

  /**
    * Implicit conversion so that the Path logic isn't
    * exposed to the outside code
    */
  implicit def toPath(filename: String) =
  FileSystems.getDefault.getPath(filename)

  /**
    * Recursively goes through the directory and finds all files inside it
    * including directories
    */
  def find(dir: Path, includeHidden: Boolean = false): Traversable[Path] =
  FileTreeWalker(dir).filter {
    case VisitFile(p, atts) => includeHidden || (!p.isHidden)
    case _ => false
  }.map {
    case VisitFile(p, atts) => p
  }

  /**
    * Finds files in the idr and returns the relative path to them.
    *
    * Note: Operations like _.isRegularFile and other don't work on relative
    * Paths!
    */
  def findRelative(dir: Path): Traversable[Path] =
  find(dir).map(dir.relativize(_)) // drop the actual file

  /**
    * Recursively goes through the directory and finds all the regular files. So
    * directories and stuff will be excluded.
    */
  def findRegularFiles(dirName: String) = find(dirName).filter(_.isRegularFile)

  /**
    * Adds a check to a Path that tells us if it is a regular file
    */
  implicit class PathImplicits(path: Path) {
    def isRegularFile = java.nio.file.Files.isRegularFile(path)

    def isHidden = java.nio.file.Files.isHidden(path)
  }

  /**
    * Adds a predicate to check if a file is a regular file
    */
  implicit class FileImplicits(f: File) {
    def isRegularFile = f.toPath().isRegularFile
  }

  def copyRecursive(from: Path, to: Path) = FileTreeWalker.copyRecursive(from, to)
}
