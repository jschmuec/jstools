package com.schmueckers.jstools

import java.nio.file.FileSystems
import scala.collection.JavaConverters._
import java.nio.file.Path
import java.io.File
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.FileVisitResult

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
  def find(dir: Path, includeHidden: Boolean = false): Traversable[Path] =
    new FileTreeWalker( dir ).filter(p => includeHidden || (!p.isHidden))

  class FileTreeWalker(val path: Path) extends Traversable[Path] {
    override def foreach[U](f: (Path) => U) {
      java.nio.file.Files.walkFileTree(path, new SimpleFileVisitor[Path]() {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          f(file)
          FileVisitResult.CONTINUE
        }
      })
    }
  }
  /** Finds files in the idr and returns the relative path to them.
    *
    * Note: Operations like _.isRegularFile and other don't work on relative
    * Paths!
    */
  def findRelative(dir: Path): Traversable[Path] =
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
