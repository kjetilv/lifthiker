package code.model

case class Position(latitude: Double, longitude: Double, altitude: Option[Double] = None) {

  private val conversion = new com.ibm.util.CoordinateConversion()
  
  private val coords = {
    val converted = conversion.latLon2UTM(latitude, longitude)
    val values = converted split "\\s+" takeRight 2 map (_.toInt)
    values(0) → values(1)
  }
  
  val x = coords._1
  
  val y = coords._2
  
  def closeTo(position: Position) = true
}
