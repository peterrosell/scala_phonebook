package net.biskvi

import net.biskvi.PhoneBook.PhoneBookEntry

class PhoneBook {

  val rootNode = new DigitNode

  def add(entry: PhoneBookEntry): Unit = {
    def add(name: String, digits: List[Int], node: DigitNode): Unit = {
      digits match {
        case Nil => throw new IllegalArgumentException("No phone number")
        case x :: Nil =>
          node.setEntry(x, entry)
        case x :: xs =>
          add(name, xs, node.digitNode(x))
      }
    }

    add(entry._1, numberAsDigitList(entry._2), rootNode)
  }

  def numberAsDigitList(number: String): List[Int] = number.toList.filter(c => c >= '0' && c <= '9').map(_ - '0')

  def entryByNumber(number: String): Option[PhoneBookEntry] = {
    def entryByNumber(digits: List[Int], node: DigitNode): Option[PhoneBookEntry] = {
      digits match {
        case x :: Nil =>
          node.entry(x)
        case x :: xs =>
          node.getDigitNode(x) match {
            case Some(digitNode) => entryByNumber(xs, digitNode)
            case _ => None
          }
        //          node.getDigitNode(x).map(b=> entryByNumber(xs,b)).flatten
        case _ => None
      }
    }

    entryByNumber(numberAsDigitList(number), rootNode)
  }

//  def indent(i: Int, writer: Writer): Unit = for (x <- 0 until i) writer.append("  ")

  override def toString = {
    rootNode.toString
/*
    def write(level: Int, node: Node, output: Writer): Unit = node match {
      case personNode: PersonNode =>
        indent(level, output)
        output.append(personNode.toString).append("\n")
      case digitNode: DigitNode =>
        digitNode.digits.foreach(write(level + 1, _, output))
      case _ =>
    }
    val sw = new StringWriter
    write(0, rootNode, sw)
    sw.toString
*/
  }
}

object PhoneBook {
  type PhoneBookEntry = (String, String)

}