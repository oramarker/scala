package com.test

import java.time.{LocalDate, LocalDateTime, LocalTime}

import org.scalatest.{FunSuite, Matchers}

class OrderedDateTimeTest extends FunSuite with Matchers {

  implicit def toComparableDateTime(time: LocalTime): ComparableDateTime = new ComparableDateTime(time)

  implicit def toComparableDateTime(date: LocalDate): ComparableDateTime = new ComparableDateTime(date)

  implicit def toComparableDateTime(datetime: LocalDateTime): ComparableDateTime = new ComparableDateTime(datetime)

  test("implicit datetime") {

    // date vs time
    val date1 = LocalDate.of(2018, 1, 1)
    val time1 = LocalTime.NOON
    date1.compare(time1) shouldBe (-1)

    // date vs date
    val date2 = LocalDate.of(2018, 1, 2)
    date2.compare(date1) shouldBe (1)

    //  date vs date time
    val dateTime1 = LocalDateTime.of(2018, 1, 1, 1, 0, 0)
    val dateTime2 = LocalDateTime.of(2018, 1, 1, 0, 0, 0)
    dateTime1.compare(date1) shouldBe (1)
    dateTime2.compare(date1) shouldBe (0)
    dateTime2.compare(date2) shouldBe (-1)
  }

}
