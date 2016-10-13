package com.schmueckers.jstools

import java.io.InputStreamReader
import com.opencsv.CSVReader
import java.io.FileInputStream

/** A wrapper around CSV file that implements [[[scala.Traversable]]] and so has all the
  * goodies.
  *
  * {{{
  * val csv = new CSVFile("data.csv")
  * for (values <- csv) {
  * // process values
  * }
  * }}}
  *
  * Not only that, but CSVFile also inherits all sorts of potentially useful methods
  * from like map, filter. etc. For example you could extract only the numbers in the
  * third column into a list with
  *
  * {{{
  * val numbers = csv.map(values => values(2).toInt)
  * }}}
  *
  * This code is a snippet from [[https://labs.encoded.io/2012/04/09/reading-csv-files-in-scala-the-traversable-way]].
  *
  * It's not the best way of actually doing this, but a good foundation I think. We need to think about csv-files
  * that contain header rows.
  */
class CSVFile(fileName: String,
              charset: String = "UTF-8",
              separator: Char = ',',
              quote: Char = '"',
              escape: Char = '\\') extends Traversable[Array[String]] {

  override def foreach[U](f: Array[String] => U): Unit = {
    val csvReader = new CSVReader(new InputStreamReader(new FileInputStream(fileName), charset), separator, quote, escape)
    try {
      var next = true
      while (next) {
        val values = csvReader.readNext()
        if (values != null) f(values)
        else next = false
      }
    } finally {
      csvReader.close()
    }
  }

  def toTable: Table[String, String] = {
    this.toList match {
      case h :: t => new Table[String, String] { def rows = t.map(_.toSeq.map(Some(_))); def headers = h }
      case Nil    => new Table[String, String] { def rows = Seq.empty; def headers = Seq.empty }
    }
  }
}

