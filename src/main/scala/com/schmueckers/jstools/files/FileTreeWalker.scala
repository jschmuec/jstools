package com.schmueckers.jstools.files

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._
import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes

trait FileVisitEvent

case class PreVisitDirectory(path: Path, atts: BasicFileAttributes) extends FileVisitEvent

case class PostVisitDirectory(dir: Path, exc: IOException) extends FileVisitEvent

case class VisitFile(file: Path, attrs: BasicFileAttributes) extends FileVisitEvent

case class VisitFileFailed(file: Path, exc: IOException) extends FileVisitEvent


/**
  * Scala style walker for a directory tree
  *
  * Is a treversable over the tree which traverses different event types extending {{FileVisitEvent}}
  *
  * @param from
  */
class FileTreeWalker(from: Path) extends Traversable[FileVisitEvent] {
  def wrapper(x: => Unit): FileVisitResult = {
    x
    FileVisitResult.CONTINUE
  }

  override def foreach[U](f: (FileVisitEvent) => U): Unit = {
    Files.walkFileTree(from, new SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, atts: BasicFileAttributes): FileVisitResult =
        wrapper {
          f(PreVisitDirectory(dir, atts))
        }

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult =
        wrapper {
          f(PostVisitDirectory(dir, exc))
        }

      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult =
        wrapper {
          f(VisitFile(file, attrs))
        }

      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult =
        wrapper {
          f(VisitFileFailed(file, exc))
        }
    })
  }
}

object FileTreeWalker {
  def apply(from: Path) = new FileTreeWalker(from)

  /**
    * Recursivley copies a directory
    *
    * @param from
    * @param to
    */
  def copyRecursive( from : Path, to : Path ) {
    new FileTreeWalker(from).foreach {
      case PreVisitDirectory(dir, atts) =>
        try {
          java.nio.file.Files.copy(dir,
            to.resolve(from.relativize(dir)),
            java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        }
        catch {
          case d: DirectoryNotEmptyException => Unit
        }
      case PostVisitDirectory(dir, exc) => ""
      case VisitFile(file, attrs) => java.nio.file.Files.copy(file,
        to.resolve(from.relativize(file)), java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      case VisitFileFailed(file, exc) => ""
    }
  }
}