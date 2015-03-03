package net.biskvi

import java.io.{StringWriter, Writer}

import net.biskvi.PhoneBook.PhoneBookEntry

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class Node {
  val isEmpty = false: Boolean
  val isPersonNode: Boolean
  val isDigitNode = !isPersonNode
}

class EmptyNode extends Node {
  override val isEmpty = true
  val isPersonNode = false
  override val isDigitNode = false
}

case class PersonNode(entry: PhoneBookEntry) extends Node {
  val isPersonNode = true

  override def toString = entry._2.toList.filter(c => c >= '0' && c <= '9').mkString + " => " + entry._1 + ": " + entry._2
}

class DigitNode extends Node {
  val isPersonNode = false
  val digits: ArrayBuffer[Node] = mutable.ArrayBuffer.fill(10)(new EmptyNode)

  def getDigitNode(i: Int): Option[DigitNode] = digits(i) match {
    case digitNode:DigitNode => Some(digitNode)
    case _ => None
  }

  def child(i: Int): Node = digits(i) match {
    case digitNode: DigitNode => digitNode
    case emptyNode: EmptyNode =>
      val digitNode = new DigitNode
      digits.update(i, digitNode)
      digitNode
    case personNode: PersonNode => personNode
  }

  def entry(i: Int): Option[PhoneBookEntry] = digits(i) match {
    case _:EmptyNode => None
    case personNode:PersonNode => Some(personNode.entry)
    case _:DigitNode=> throw new Exception("BUG! DigitNode don't have an entry.")
  }

  /**
   * Set entry at index i. If a conflict is found all conflicting entries are returned and
   * the new entry is NOT added.
   *
   * @return all conflicting entries
   */
  def setEntry(i: Int, entry: PhoneBookEntry): Set[PhoneBookEntry] = if (digits(i).isEmpty) {
    digits.update(i, PersonNode(entry))
    Set.empty
  } else {
//    println("SET-CONFLICT--------------> " + entry)
    getEntries(digits(i))
  }

  def getEntries(node: Node): Set[PhoneBookEntry] = {
    node match {
      case digitNode: DigitNode =>
        digitNode.digits.map(b => getEntries(b)).flatten.toSet
      case personNode: PersonNode => Set(personNode.entry)
      case _ => Set.empty
    }
  }


  def indent(i: Int, writer: Writer): Unit = for (x <- 0 until i) writer.append("  ")

  override def toString = {
    def write(level: Int, node: Node, output: Writer): Unit = node match {
      case personNode: PersonNode =>
        //        indent(level, output)
        output.append(personNode.toString).append("\n")
      case digitNode: DigitNode =>
        digitNode.digits.foreach(write(level + 1, _, output))
      case _ =>
    }
    val sw = new StringWriter
    write(0, this, sw)
    sw.toString
  }
}