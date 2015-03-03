package net.biskvi

import java.text.ParseException

import net.biskvi.PhoneBook._
import org.scalatest.{FlatSpec, ShouldMatchers}

import scala.io.Source


class PhoneListTest extends FlatSpec with ShouldMatchers {
  
 def time[A](f: => A)(implicit iterations:Int = 1) = {
    val s = System.nanoTime
    for( i <- 0 until iterations) {
      f
    }
    println(s"Average time of $iterations iterations: " + (System.nanoTime - s) / 1e6 / iterations + "ms")
  }

  "Test environment" should "warmup with read a file of 65535 entries" in {
    val persons = readPersons("/phone_data_65535.txt")

    time {
      val phoneBook = new PhoneBook
      persons.map(phoneBook.add).force
      phoneBook.adds should be(65535)
      phoneBook.conflicts.size should be(94)
      //    phoneBook.conflicts.map(println)
    }(100)

  }


  "A phone list" should "not be consistent with two entries with number 1 and 12" in time {
    PhoneList.isConsistent(Stream("bob" -> "1", "alice" -> "12")) should be(false)
  }

  it should "not be consistent with two entries with number 12 and 1" in time {
    PhoneList.isConsistent(Stream("alice" -> "12", "bob" -> "1")) should be(false)
  }

  it should "be consistent with one entry" in time {
    PhoneList.isConsistent(Stream("bob" -> "1")) should be(true)
  }

  it should "not be consistent with two entries with same number" in time {
    PhoneList.isConsistent(Stream("bob" -> "1", "alice" -> "1")) should be(false)
  }

  it should "not be consistent with three entries" in time {
    PhoneList.isConsistent(Stream("bob" -> "91 12 54 26", "alice" -> "97 625 992", "emergency" -> "911")) should be(false)
  }

  "A phone list" should "be consistent from phone_data.txt" in time {
    PhoneList.isConsistent(getClass.getResource("/phone_data.txt")) should be(true)
  }(100)

  it should "not be consistent from phone_data_10000.txt" in time {
    PhoneList.isConsistent(getClass.getResource("/phone_data_10000.txt")) should be(false)
  }(100)

  it should "not be consistent from phone_data_65535.txt" in time {
    PhoneList.isConsistent(getClass.getResource("/phone_data_65535.txt")) should be(false)
  }(100)

  "A PhoneBook" should "contain one entry" in {
    val phoneBook = new PhoneBook
    val number = "1"
    phoneBook.add("bob" -> number)
    phoneBook.entryByNumber(number).get._1 should be("bob")
    phoneBook.entryByNumber(number).get._2 should be("1")
  }

  it should "contain two entries" in {
    val phoneBook = new PhoneBook
    val bobNumber = "12"
    val aliceNumber = "23"
    Stream("bob" -> bobNumber, "alice" -> aliceNumber).foreach(phoneBook.add)

    phoneBook.entryByNumber(bobNumber).get._1 should be("bob")
    phoneBook.entryByNumber(bobNumber).get._2 should be("12")

    phoneBook.entryByNumber(aliceNumber).get._1 should be("alice")
    phoneBook.entryByNumber(aliceNumber).get._2 should be("23")
  }

  it should "contain two entries starting with same digit" in {
    val phoneBook = new PhoneBook
    val bobNumber = "12"
    val aliceNumber = "13"
    Stream("bob" -> bobNumber,"alice" -> aliceNumber).foreach(phoneBook.add)

    phoneBook.entryByNumber(bobNumber).get._1 should be("bob")
    phoneBook.entryByNumber(bobNumber).get._2 should be("12")

    phoneBook.entryByNumber(aliceNumber).get._1 should be("alice")
    phoneBook.entryByNumber(aliceNumber).get._2 should be("13")
  }


  it should "read a file of 1000 entries" in {
    val persons = readPersons("/phone_data.txt")

    time {
      val phoneBook = new PhoneBook
      persons.map(phoneBook.add).force
      phoneBook.adds should be(1000)
      phoneBook.conflicts.size should be(0)
      //    phoneBook.conflicts.map(println)
    }(100)


  }

  it should "read a file of 10000 entries" in {
    val persons = readPersons("/phone_data_10000.txt")

    time {
      val phoneBook = new PhoneBook
      persons.map(phoneBook.add).force
      phoneBook.adds should be(10000)
      phoneBook.conflicts.size should be(2)
      //    phoneBook.conflicts.map(println)
    }(100)


  }

  it should "read a file of 65535 entries" in {
    val persons = readPersons("/phone_data_65535.txt")

    time {
      val phoneBook = new PhoneBook
      persons.map(phoneBook.add).force
      phoneBook.adds should be(65535)
      phoneBook.conflicts.size should be(94)
      //    phoneBook.conflicts.map(println)
    }(100)

  }

  def readPersons(url: String): Stream[(String, String)] = {
    val lines = Source.fromURL(getClass.getResource(url)).getLines()
    lines.next()
    lines.map(PhoneList.parseLine).toStream
  }
}
