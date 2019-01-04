
sealed abstract class CalculationTree

case class Leaf(value: Int) extends CalculationTree

case class Node(op: Char, leaves: CalculationTree*) extends CalculationTree {

  def eval(): Double = {
    var leavesValues: List[Double] = leaves
      .collect { case leaf: Leaf => leaf.value.toDouble }
      .toList
    for (leaf <- leaves) {
      leaf match {
        case node: Node =>
          leavesValues =  node.eval() :: leavesValues
        case _ =>
      }
    }
    arithmeticOperations(leavesValues.reverse)
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
    print("Op: %s Values: %s Result: %s\n".format(op, leavesValues.mkString(","), res))
    res
  }
}

object CalculationTreeMain {

  //(((6*2)+3+4)/(-5)
  def main(args: Array[String]): Unit = {
    val calculus =
      Node('/',
        Node('+',
          Node('*', Leaf(6), Leaf(2)),
          Leaf(3),
          Leaf(4)),
        Node('-', Leaf(5))
      )
    print(calculus.eval())
  }
}


