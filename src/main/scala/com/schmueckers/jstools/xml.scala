package com.schmueckers.tools

import scala.xml._

/** Allows to fold and transform trees
  */
package object xml {

  /** TreeTransformer allows to transform the nodes of a tree
    */
  object TreeTransformer {
    /** Returns a function which applies a transformation function
      * to the argument node and all it's child nodes
      *
      * @param get_children a function which returns the children for a given node
      *
      * @param the transformation function which will be applied to all nodes and
      * childnodes. The transformed child nodes will be passed into this function,
      * thereby the transformation function can decide if the children should be
      * transformed or not.
      *
      * @return a [[Seq]] of nodes that is the result of transforming the argument
      * node and all the child nodes
      */
    def apply[S, N](get_children: (N) => Seq[N],
                    transform: ((S, N, (S, Seq[N])) => (S, Seq[N]))) = {
      def f(state_nodes: (S, Seq[N]), n: N): (S, Seq[N]) = {
        val (state, nodes) = state_nodes
        val children = get_children(n)
        val cr = children.foldLeft((state, Seq.empty[N]))(f)
        val r = transform(state, n, cr)
        (r._1, nodes ++ r._2)
      }
      f _
    }
  }

  /** An implicit class extension that allows to transform [[scala.xml.Node]] through
    * [[#transform]]
    */
  implicit class XmlTransformer(n: Node) {
    /** Transform a {{scala.xml.Node}} with full state
      */
    def transform[S](transformer: (S, Node) => (S, Seq[Node]))(s: S) = {
      def compose(s: S, n: Node, cr: (S, Seq[Node])) = n match {
        case Elem(prefix, label, attributes, scope, chs @ _*) => {
          val r = transformer(cr._1,
            Elem(n.prefix, n.label, n.attributes, n.scope, cr._2.isEmpty, cr._2: _*))
          r
        }
        case default => transformer(s, n)
      }
      def get_children(n: Node) = n.child

      TreeTransformer(get_children, compose)((s, Seq.empty[Node]), n)
    }
    /** Map a {{scala.xml.Node}} without keeping state
      */
    def tmap(transformer: Node => Seq[Node]): Seq[Node] = {
      def wrapper(s: Int, n: Node): (Int, Seq[Node]) = (0, transformer(n))
      val w = wrapper _
      val r = transform(w)(0)
      r._2
    }
    /** Fold depth first over the tree of an {{scala.xml.node}}
      */
    def foldDF[S](f: (S, Node) => S)(s: S): S = {
      def wrapper(s: S, n: Node): (S, Seq[Node]) = (f(s, n), Seq.empty[Node])
      val r = transform(wrapper _)(s)
      r._1
    }
  }
}
