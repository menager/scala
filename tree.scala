/*Homework 8 Name: Morgan Nager*/
package scala

object tree {
  
  def main (arg: Array[String]){
    val input = List(3, 4, 2, 10, 9, 1, 5, 6, 11, 12, 13, 14, 15, 16,17,18,19,20)
    .map(i=>OrderedInt(i))
    val t = input.foldLeft[Tree[OrderedInt]](Leaf[OrderedInt]())((l, i) => l.insert(i))
    val n = t.asInstanceOf[Node[OrderedInt]]
    val t1 = n.left.asInstanceOf[Node[OrderedInt]];
    val t2 = n.right.asInstanceOf[Node[OrderedInt]];
    println("t0 =\t(t1, " + n.x+":"+n.color + ", t2)")
    println("t1 =\t(t3, " + t1.x+":"+t1.color+", t4)")
    println("t2 =\t(t5, " + t2.x+":"+t2.color+", t6)")
    println("t3 =\t" + t1.left)
    println("t4 =\t" + t1.right)
    println("t5 =\t" + t2.left)
    println("t6 =\t" + t2.right)
  }
  
 case class OrderedInt(i:Int) extends Ordered[OrderedInt]{
    def compare(that: OrderedInt) = this.i - that.i
    override def toString = (i.toString())
  }
  
  trait Tree[X]{
    def contains(e:X):Boolean
    def ins(e:X): Tree[X]
    
    def insert(e:X)(implicit ord: Ordering[X]): Tree[X] =
      ins(e) match{
        case Leaf() => Node(Red, e, Leaf(), Leaf())
        case Node(_, x, y, z) => Node(Black, x, y, z)
      }

   }
  
  trait Color{}
    case object Red extends Color{ override def toString = "R"}
    case object Black extends Color{ override def toString = "B"}
      

  case class Leaf[X <% Ordered[X]]() extends Tree[X]{
    def contains(e:X) = false
    def ins(e:X) = {
      Node(Red, e, Leaf(), Leaf())
    }
    
    override def toString ="L"
  }
  
  case class Node[X <% Ordered[X]](color: Color, x: X, left: Tree[X], right: Tree[X]) extends Tree[X] {
    def contains(e: X) ={
      if (e == x) true
      else if (e < x) left.contains(e)
      else right.contains(e)
    }
    override def ins(e:X)= {
      if (e == x) this
      else if (e < x) Node(color, x, left.ins(e), right).balance
      else Node(color, x, left, right.ins(e)).balance
    }
    
     def balance ={
     
      this match{ 
        case Node(Black, z, Node(Red, y, Node(Red, x, a, b), c), d) =>
          Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        case Node(Black, z, Node(Red, x, a, Node(Red, y, b, c)), d) => 
          Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        case Node(Black, x, a, Node(Red, z, Node(Red, y, b, c), d)) =>
          Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        case Node(Black, x, a, Node(Red, y, b, Node(Red, z, c, d))) => 
          Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
        case _ => this
       
        }
      }
    
    override def toString = "(" + left + ", " + x + ":" + color + ", " + right + ")"
  }
  
 }
