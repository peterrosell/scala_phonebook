package net.biskvi

import java.io.File
import java.net.URL
import java.text.ParseException

import scala.io.Source

object PhoneList {
  import PhoneBook.PhoneBookEntry

  def parseLine(s: String): PhoneBookEntry = s.split(",").toList match {
    case name::number::Nil => (name,number)
    case _ => throw new ParseException("Invalid data: " + s, -1)
  }


  def isConsistent( url:URL) :Boolean = isConsistent(Source.fromURL(url))
  def isConsistent(file:File):Boolean = isConsistent(Source.fromFile(file))
  def isConsistent(source:Source):Boolean = isConsistent(source.getLines().filterNot(_.contains("Phone Number")).map(parseLine).toStream)

  def isConsistent(persons: Stream[PhoneBookEntry]):Boolean = try {
    persons.foreach((new PhoneBook).add)
    true
  }
  catch {
    case e:IllegalArgumentException =>
      println(e.getMessage)
      false
  }
}
