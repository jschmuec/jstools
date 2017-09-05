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

  override def foreach[U](f: (FileVisitEvent) => U): Unit =
    walk((fve: FileVisitEvent) => {
      f(fve)
      FileVisitResult.CONTINUE
    })

  def walk(f: (FileVisitEvent) => FileVisitResult) = {
    Files.walkFileTree(from, new SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult =
        f(PreVisitDirectory(dir, attrs))

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult =
        f(PostVisitDirectory(dir, exc))

      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult =
        f(VisitFile(file, attrs))

      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult =
        f(VisitFileFailed(file, exc))
    })
  }

  /**
    * Runs through the tree and can filter out subtrees
    *
    * @param dirFilter A function that has to return true if children of directory
    *                  should be included
    * @return A Traversable that allows all the usual iterations
    */
  def filterSubTrees(dirFilter: (Path, BasicFileAttributes) => Boolean) = {
    new Traversable[FileVisitEvent] {
      override def foreach[U](f: (FileVisitEvent) => U): Unit =
        FileTreeWalker.this.walk(
          (fve: FileVisitEvent) => {
            f(fve)
            fve match {
              case PreVisitDirectory( path, attrs ) if dirFilter(path,attrs) =>
                FileVisitResult.SKIP_SUBTREE
              case _ =>
                FileVisitResult.CONTINUE
            }
          }
        )
    }
  }
}

object FileTreeWalker {
  def apply(from: Path) = new FileTreeWalker(from)

  /**
    * Recursivley copies a directory
    */
  def copyRecursive(from: Path, to: Path) = {
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

  def deleteRecursive(from: Path) = {
    FileTreeWalker(from) foreach {
      case VisitFile(f, atts) => java.nio.file.Files.delete(f)
      case PostVisitDirectory(d, exc) => java.nio.file.Files.delete(d)
      case _ => Unit
    }
  }
}