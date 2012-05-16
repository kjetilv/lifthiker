package code.model

import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTimeZone, DateTime, ReadableInstant}

object Conversions {

  private val argumentPattern = DateTimeFormat forPattern "ddMMyyyyHHmm"

  private val DateRegexp = """Date\((\d+)\+(\d+)\)""".r
  
  private val timeZone = DateTimeZone.forID("Europe/Oslo")

  def toArgument(date: ReadableInstant) = argumentPattern print date
  
  def fromValue(string: String) = { 
    val DateRegexp(epoch, _) = string
    new DateTime(epoch.toLong).withZone(timeZone)
  }
}

case class Stops(position: Position,
                 stops: List[Stop],
                 walkingDistance: WalkingDistance) {
  
  val hits = stops.size
  
  def scaledTo(hits: Int, distance: WalkingDistance) = 
    Stops(
      position, 
      stops take hits filter (_.WalkingDistance < walkingDistance.meters), 
      distance
    )
  
  def farFrom(position: Position) = !closeTo(position)
  
  def closeTo(position: Position) = this.position closeTo position
}

case class Stop(ID: Int,
                Name: String,
                District: String,
                Type: Int,
                X: Int,
                Y: Int,
                ShortName: String,
                WalkingDistance: Int, 
                Lines: List[Line]) {

  private val conversion = new com.ibm.util.CoordinateConversion
  
  private val latLon = {
    conversion.utm2LatLon("32 V " + X + " " + Y)
  } 
  
  val latitude = latLon(0)
  
  val longitude = latLon(1)
}

case class Line(LineID: Int,
                LineName: String,
                Transportation: Int) {
  
  val mode = Transportation match {
    case 7 => "Trikk"
    case 2 => "Buss"
    case x => x.toString
  }
}
