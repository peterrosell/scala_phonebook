package net.biskvi

import org.scalatest.{ShouldMatchers, FlatSpec}

class PhoneListTest extends FlatSpec with ShouldMatchers {

  behavior of "A PhoneList"

  it should "be consistent with one entry" in {
    PhoneList.isConsistent(Stream("bob" -> "1")) should be(true)
  }

  it should "not be consistent with two entries with same number" in {
    PhoneList.isConsistent(Stream("bob" -> "1","alice"->"1")) should be(false)
  }

  it should "not be consistent with two entries with number 1 and 12" in {
    PhoneList.isConsistent(Stream("bob" -> "1","alice"->"12")) should be(false)
  }

  it should "not be consistent with three entries" in {
    PhoneList.isConsistent(Stream("bob" -> "91 12 54 26","alice"->"97 625 992", "emergency"->"911")) should be(false)
  }

  it should "not be consistent from phone_data.txt" in {
    PhoneList.isConsistent(getClass.getResource("/phone_data.txt")) should be(true)
  }

  it should "not be consistent from phone_data_10000.txt" in {
    PhoneList.isConsistent(getClass.getResource("/phone_data_10000.txt")) should be(false)
  }

  it should "not be consistent from phone_data_65535.txt" in {
    PhoneList.isConsistent(getClass.getResource("/phone_data_65535.txt")) should be(false)
  }

  behavior of "A PhoneBook"

  it should "contain one entry" in {
    val phoneBook = new PhoneBook
    val number = "1"
    phoneBook.add("bob" -> number)
    phoneBook.entryByNumber(number).get._1 should be("bob")
    phoneBook.entryByNumber(number).get._2 should be( "1")
  }

  it should "contain two entries" in {
    val phoneBook = new PhoneBook
    val bobNumber = "12"
    val aliceNumber = "23"
    Stream("bob" -> bobNumber,"alice" -> aliceNumber).foreach(phoneBook.add)

    phoneBook.entryByNumber(bobNumber).get._1 should be("bob")
    phoneBook.entryByNumber(bobNumber).get._2 should be( "12")

    phoneBook.entryByNumber(aliceNumber).get._1 should be("alice")
    phoneBook.entryByNumber(aliceNumber).get._2 should be( "23")
  }

}
