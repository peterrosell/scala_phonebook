package net.biskvi

import java.io.{StringWriter, Writer}

import net.biskvi.PhoneBook.PhoneBookEntry

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class Node {
  val isEmpty: Boolean
  val isPersonNode: Boolean
  val isDigitNode = !isPersonNode
}

class EmptyNode extends Node {
  val isEmpty = true
  val isPersonNode = false
  override val isDigitNode = false
}

case class PersonNode(entry: PhoneBookEntry) extends Node {
  val isEmpty = false
  val isPersonNode = true
  override def toString = entry._1 + ": " + entry._2
}

class DigitNode extends Node {
  def getDigitNode(i: Int) :Option[DigitNode] = if(digits(i).isDigitNode)
    Some(digits(i).asInstanceOf[DigitNode])
  else
    None

  def digitNode(i: Int): DigitNode = digits(i) match {
    case digitNode: DigitNode => digitNode
    case emptyNode: EmptyNode => digits.update(i, new DigitNode); digits(i).asInstanceOf[DigitNode]
    case personNode: PersonNode => throw new IllegalArgumentException("Can't add entry due to conflict with " + personNode)
  }

  val isEmpty = false

  def hasEntryAt(i: Int) = digits(i).isPersonNode

  def entry(i: Int): Option[PhoneBookEntry] = if (!digits(i).isEmpty)
    Some(digits(i).asInstanceOf[PersonNode].entry)
  else
    None

  def setEntry(i: Int, entry: PhoneBookEntry): Unit = if (digits(i).isEmpty)
    digits.update(i, PersonNode(entry))
  else
    throw new IllegalArgumentException(entry + " can't be added due to collision with " + digits(i))


  val isPersonNode = false
  collection.mutable.ArrayBuffer.fill(100)(-1)
  val digits: ArrayBuffer[Node] = mutable.ArrayBuffer.fill(10)(new EmptyNode)

  def indent(i: Int, writer: Writer): Unit = for (x <- 0 until i) writer.append("  ")

  override def toString = {
      def write(level: Int, node: Node, output: Writer): Unit = node match {
        case personNode: PersonNode =>
          indent(level, output)
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