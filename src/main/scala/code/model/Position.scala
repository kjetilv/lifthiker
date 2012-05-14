package code.model

case class Position(latitude: Double, longitude: Double, altitude: Option[Double] = None) {

  private val conversion = new com.ibm.util.CoordinateConversion()
  
  val x, y: (Int, Int) = {
    val converted = conversion.latLon2UTM(latitude, longitude)
    val values = converted split "\\s+" takeRight 2 map (_.toInt)
    values(0) → values(1)
  }
}
