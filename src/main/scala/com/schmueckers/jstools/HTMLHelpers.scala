package com.schmueckers.jstools

/**
 * Defines various stuff that makes working with HTML easier
 */
package object HTMLHelpers {


  /**
   * A small function that escapes & with &amp;
   */
  def escape_amp(s: String) =
    "&(?!.{1,4};)".r.replaceAllIn(s, "&amp;")

  implicit class ElemExtension(elem: scala.xml.Elem) {
    /**
     * Adds an attribute to an [[scala.xml.Elem]]
     */
    def add_attribute(key: String, value: String): scala.xml.Elem =
      elem.copy(attributes = new scala.xml.UnprefixedAttribute(key, value, elem.attributes))
  }
}
