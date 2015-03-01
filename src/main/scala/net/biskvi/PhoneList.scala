package net.biskvi

import java.io.File
import java.net.URL
import java.text.ParseException

import scala.io.Source

object PhoneList {

  import PhoneBook.PhoneBookEntry

  def parseLine(s: String): PhoneBookEntry = s.split(",").toList match {
    case name :: number :: Nil => (name, number)
    case _ => throw new ParseException("Invalid data: " + s, -1)
  }

  def isConsistent(url: URL): Boolean = isConsistent(Source.fromURL(url))

  def isConsistent(file: File): Boolean= isConsistent(Source.fromFile(file))

  def isConsistent(source: Source): Boolean = isConsistent(source.getLines().filterNot(_.contains("Phone Number")).map(parseLine).toStream)

  def isConsistent(persons: Stream[PhoneBookEntry]): Boolean = {
    val phoneBook = new PhoneBook
    persons.map(phoneBook.add).takeWhile(b=>b).force
    println( "nr of adds:" + phoneBook.adds)
    phoneBook.conflicts.isEmpty
  }

  def add(phoneBook:PhoneBook, phoneBookEntry: PhoneBookEntry) :Option[String] = {
    try {
      phoneBook.add(phoneBookEntry)
      None
    } catch {
      case e: Throwable =>
        Some(e.getMessage)
    }
  }

  def showInconsistentEntries(url: URL): Stream[Option[String]] = showInconsistentEntries(Source.fromURL(url))

  def showInconsistentEntries(file: File): Stream[Option[String]] = showInconsistentEntries(Source.fromFile(file))

  def showInconsistentEntries(source: Source): Stream[Option[String]] = showInconsistentEntries(source.getLines().filterNot(_.contains("Phone Number")).map(parseLine).toStream)

  def showInconsistentEntries(persons:Stream[PhoneBookEntry]) : Stream[Option[String]] = {
    val phoneBook = new PhoneBook
    persons.map(add(phoneBook,_)).filter(_.isDefined)
  }

  def catchAndLog(f: => Unit) = {
    try {
      f
    } catch {
      case e: Throwable => Some(e.getMessage)
    }
    None
  }

}
