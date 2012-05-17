package code.model

import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTimeZone, DateTime, ReadableInstant}
import net.liftweb.common.{Full, Box, Empty}

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

case class Stops(position: Position, stops: List[Stop], walkingDistance: WalkingDistance) {
  
  def forRoute(routeBox: Box[Int]): Stops = {
    routeBox match {
      case Full(route) => Stops(position, stops map withLinesFor(route) filter (hasLines), walkingDistance)
      case Empty => this
    }
  }

  private def withLinesFor(route: Int) = (stop: Stop) => stop.copy(Lines = stop.Lines filter lineFor(route)) 

  private def lineFor(route: Int) = (line: Line) => line.LineID == route
  
  private def hasLines = (stop: Stop) => !stop.Lines.isEmpty
  
  val hits = stops.size
  
  def scaledTo(hits: Int, distance: WalkingDistance) = 
    Stops(position, 
      stops sortBy (_.WalkingDistance) take hits filter withinDistance, 
      distance
    )

  def withinDistance: (Stop) => Boolean = _.WalkingDistance < walkingDistance.meters

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
