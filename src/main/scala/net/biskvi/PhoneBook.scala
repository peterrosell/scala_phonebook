package net.biskvi

import net.biskvi.PhoneBook.PhoneBookEntry

import scala.collection.parallel.mutable
import scala.collection.parallel.mutable.ParSet

class PhoneBook {

  val rootNode = new DigitNode
  val conflicts: mutable.ParSet[Conflict] = ParSet.empty
  var adds = 0

  def add(entry: PhoneBookEntry): Boolean = {
    def add(name: String, digits: List[Int], node: DigitNode): Boolean = {
      digits match {
        case Nil => throw new IllegalArgumentException("No phone number")
        case x :: Nil =>
          val entryConflicts = node.setEntry(x, entry)
          if (entryConflicts.nonEmpty) {
            entryConflicts.map(Conflict(entry,_)).foreach(conflicts += _)
            false
          } else {
            true
          }
        case x :: xs =>
          node.child(x) match {
            case person: PersonNode =>
//              println("CONFLICT--------------> " + entry)
              conflicts += Conflict(entry, person.entry)
              false
            case digitNode:DigitNode =>
              add(name, xs, digitNode)
            case emptyNode:EmptyNode =>
              throw new Exception("BUG! Shall never get an empty node.")
          }
      }
    }

    adds += 1
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
        case _ => None
      }
    }

    entryByNumber(numberAsDigitList(number), rootNode)
  }

  override def toString = rootNode.toString
}

object PhoneBook {
  type PhoneBookEntry = (String, String)

}