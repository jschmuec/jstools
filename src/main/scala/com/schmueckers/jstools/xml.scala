package com.schmueckers

import scala.xml._

/*
implicit final class MLPipe[T](val x: T) extends AnyVal {
  def |>[B](f: (T) => B) = f(x)
}
*/

/*
 *  
 */
/*
object XmlTransformer {
  type StatefullTransformer[S] = (S, Node) => (S, Seq[Node])
  type StatelessTransformer = Node => Seq[Node]

  def apply(t: (Node) => Seq[Node]) = (n: Node) =>
    new SimpleTransformer(t).u(n)

  implicit class XmlTransformations(ns: Seq[Node]) {
    def transform[S](s: S)(t: (S, Node) => (S, Seq[Node])) =
      new RuleTransformer2(t).use(s, ns)
    def transform(t: (Node) => Seq[Node]) = new SimpleTransformer(t).use(Unit, ns)._2
  }
  implicit class XmlTransformation(n: Node) {
    def transform[S](s: S)(t: StatefullTransformer[S]) = Seq(n).transform(s)(t)
    def transform(t: StatelessTransformer) = Seq(n).transform(t)
    def fold[S](s: S, f: (S, Node) => S) = {
      def side_effect_only(s: S, n: Node): (S, Seq[Node]) = (f(s, n), Nil)
      transform(s)(side_effect_only)
    }
  }
}
*/

/**
 * Allows to fold and transform trees
 */

package object tools {

  /**
   * TreeTransformer allows to transform the nodes of a tree
   */
  object TreeTransformer {
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

  /**
   * An implicit class extension that allows to transform [[scala.xml.Node]] through
   * [[#transform]]
   */
  implicit class XmlTransformer(n: Node) {
    /**
     * Transform a {{scala.xml.Node}} with full state
     */
    def transform[S](transformer: (S, Node) => (S, Seq[Node]))(s: S) = {
      def compose(s: S, n: Node, cr: (S, Seq[Node])) = n match {
        case t: Text => transformer(s, n)
        case Elem(prefix, label, attributes, scope, chs @ _*) => {
          val r = transformer(cr._1,
            Elem(n.prefix, n.label, n.attributes, n.scope, cr._2.isEmpty, cr._2: _*))
          r
        }
      }
      def get_children(n: Node) = n.child

      TreeTransformer(get_children, compose)((s, Seq.empty[Node]), n)
    }
    /**
     * Map a {{scala.xml.Node}} without keeping state
     */
    def tmap(transformer: Node => Seq[Node]): Seq[Node] = {
      def wrapper(s: Int, n: Node): (Int, Seq[Node]) = (0, transformer(n))
      val w = wrapper _
      val r = transform(w)(0)
      r._2
    }
    /**
     * Fold depth first over the tree of an {{scala.xml.node}} 
     */
    def foldDF[S](f: (S, Node) => S)(s: S): S = {
      def wrapper(s: S, n: Node): (S, Seq[Node]) = (f(s, n), Seq.empty[Node])
      val r = transform(wrapper _)(s)
      r._1
    }
  }
}
