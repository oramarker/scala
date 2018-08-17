package com.test

import java.time.{LocalDate, LocalTime}

case class ComparableDateTime private(optionDate:Option[LocalDate], optionTime:Option[LocalTime]) extends Ordered[ComparableDateTime] {
  def this(date:LocalDate){
    this(Some(date),Some(LocalTime.MIDNIGHT))
  }
  def this(time:LocalTime){
    this(None,Some(time))
  }
  def this(dateTime:java.time.LocalDateTime){
    this(Some(dateTime.toLocalDate),Some(dateTime.toLocalTime))
  }
  override def compare(that: ComparableDateTime): Int = (this,that) match {
    // date time comparison
    case (ComparableDateTime(Some(date1),Some(time1)) ,ComparableDateTime(Some(date2),Some(time2))) if (date1.compareTo(date2) ==0) => time1.compareTo(time2)
    case (ComparableDateTime(Some(date1),Some(time1)) ,ComparableDateTime(Some(date2),Some(time2))) => date1.compareTo(date2)
    // time only
    case (ComparableDateTime(_,Some(time1)) ,ComparableDateTime(_,Some(time2))) => time1.compareTo(time2)
   // case (UnifiedDateTime(_,Some(time1)) ,UnifiedDateTime(None,Some(time2))) => time1.compareTo(time2)

  }
}

