import scala.collection.mutable.ListBuffer

sealed abstract class CalculationTree

case class Leaf(value: Int) extends CalculationTree

case class Node(op: Char, leaves: CalculationTree*) extends CalculationTree {

  def calcToString(actual: String = ""): String = {
    var result: String = actual
    for (leaf <- leaves) {
      leaf match {
        case node: Node =>
          result = handleArithmeticOperations(node, result)
        case _ =>
      }
    }
    handleSingleLeaf(result)
  }

  private def handleArithmeticOperations(node: Node, actual: String): String =
    if (node.leaves.size > 1)
      "(%s)%s".format(node.calcToString(actual), op)
    else
      "%s".format(node.calcToString(actual))


  private def handleSingleLeaf(actual: String): String = {
    val leavesValues = getLeavesValues.map(_.toInt)
    if (leavesValues.size == 1) {
      op match {
        case '-' =>
          "%s(%s%s)".format(actual, op, leavesValues.head)
        case _ =>
          "%s%s%s".format(actual, op, leavesValues.mkString(op.toString))
      }
    } else
      "%s%s".format(actual, leavesValues.mkString(op.toString))
  }

  def eval(): Double = {
    val leavesValues: ListBuffer[Double] = ListBuffer(getLeavesValues: _*)
    for (leaf <- leaves) {
      leaf match {
        case node: Node =>
          leavesValues.append(node.eval())
        case _ =>
      }
    }
    arithmeticOperations(leavesValues.toList)
  }

  private def arithmeticOperations(leavesValues: List[Double]): Double = {
    val res = op match {
      case '+' => leavesValues.sum
      case '-' =>
        if (leavesValues.size == 1)
          leavesValues.head.unary_-
        else leavesValues.reduce(_ - _)
      case '*' => leavesValues.product
      case '/' => leavesValues.reduce(_ / _)
      case _ => 0.0
    }
    res
  }

  private def getLeavesValues =
    leaves
      .collect { case leaf: Leaf => leaf.value.toDouble }

}

object CalculationTreeMain {

  def main(args: Array[String]): Unit = {
    val calculus =
      Node('/',
        Node('+',
          Node('*', Leaf(6), Leaf(2)),
          Leaf(3),
          Leaf(4)),
        Node('-', Leaf(5))
      )
    assert("((6*2)+3+4)/(-5)" == calculus.calcToString(), "Wrong calcToString calculus")
    assert(-3.8 == calculus.eval(), "Wrong eval calculus")

    val calculus1 =
      Node('/',
        Node('+',
          Leaf(3),
          Leaf(4)),
        Node('-', Leaf(5))
      )
    assert("(3+4)/(-5)" == calculus1.calcToString(), "Wrong calcToString calculus1")
    assert(-1.4 == calculus1.eval(), "Wrong calcToString calculus1")

    val calculus2 =
      Node('+',
        Node('-', Leaf(5)),
        Leaf(3)
      )
    assert("(-5)+3" == calculus2.calcToString(), "Wrong calcToString calculus2")
    assert(-2.0 == calculus2.eval(), "Wrong eval calculus2")

    val calculus3 =
      Node('+',
        Leaf(3),
        Leaf(4),
        Leaf(5),
        Leaf(6),
        Leaf(7),
        Leaf(8),
        Leaf(9),
      )
    assert("3+4+5+6+7+8+9" == calculus3.calcToString(), "Wrong calcToString calculus3")
    assert(42.0 == calculus3.eval(), "Wrong eval calculus3")
  }
}




