package code.model

case class Stop(ID: Int,
                Name: String,
                District: String,
                Type: Int,
                X: Int,
                Y: Int,
                ShortName: String,
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
                Transportation: Int)
