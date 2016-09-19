package com.schmueckers.jstools

/** Defines various stuff that makes working with HTML easier
  */
package object HTMLHelpers {
  /**
   * Defines a method [[#toHtml]] that translates a table into an HTML table
   */
  implicit class TableHelper[H, V](t: Table[H, V]) {
    def toHtml = <table>
                   <thead>
                     <tr>
                       { for (h <- t.headers) yield <th>{ h }</th> }
                     </tr>
                   </thead>
                   <tbody>
                     {
                       for (r <- t.rows) yield <tr>
                                                 { for (f <- r) yield <td>{ f.getOrElse("") }</td> }
                                               </tr>
                     }
                   </tbody>
                 </table>
  }
}