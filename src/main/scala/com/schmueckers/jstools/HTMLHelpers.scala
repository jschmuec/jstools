package com.schmueckers.jstools

/** Defines various stuff that makes working with HTML easier
  */
package object HTMLHelpers {
  /** Defines a method [[#toHtml]] that translates a table into an HTML table
    */
  implicit class TableHelper[H, V](t: Table[H, V]) {
    private def field2Node(f: Any) = f match {
      case s: String => scala.xml.Unparsed(s)
      case default   => default
    }

    /**
     * Exports a [[Table]] as an HTML Table
     * 
     * If the table contains string values, these will be included Unparsed in the
     * text. In particular this means that a field with the content "<a href=.." 
     * will actually result in the link being inserted in the document and not just the
     * above text.
     */
    def toHtml = {
      def v = <table>
                <thead>
                  <tr>
                    { for (h <- t.headers) yield <th>{ field2Node(h) }</th> }
                  </tr>
                </thead>
                <tbody>
                  {
                    for (r <- t.rows) yield <tr>
                                              {
                                                for (f <- r)
                                                  yield <td>{
                                                  f.map(field2Node)
                                                    .getOrElse("&nbsp;")
                                                }</td>
                                              }
                                            </tr>
                  }
                </tbody>
              </table>
      v
    }
  }
  
  /**
   * A small function that escapes & with &amp;
   */
  def escape_amp(s: String) =
    "&(?!.{1,4};)".r.replaceAllIn(s, "&amp;")
}
