/*
 * Copyright 2012 Kjetil Valstadsve
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package code.model

object Position {

  private val conversion = new com.ibm.util.CoordinateConversion()
  
  def apply(utmX: Int, utmY: Int): Position = {
    val latLon = conversion.utm2LatLon("32 V " + utmX + " " + utmY)
    Position(latLon(0), latLon(1), utmX, utmY)
  }
  
  def apply(latitude: Double, longitude: Double): Position = {
    val xy = {
        val converted = conversion.latLon2UTM(latitude, longitude)
        val values = converted split "\\s+" takeRight 2 map (_.toInt)
        values(0) → values(1)
      }
    Position(latitude, longitude, xy._1, xy._2)
  } 
}

case class Position(latitude: Double, longitude: Double, utmX: Int, utmY: Int) {

  private val earthRadiusMeters = 6371000 
  
  def closeTo(position: Position, closeMeters: Int = 100) = distanceTo(position) < closeMeters;

  def distanceTo(position: Position): Double = {
    import math._
    val dLat = (position.latitude - latitude).toRadians
    val dLon = (position.longitude - longitude).toRadians
    val lat2Rad = position.latitude.toRadians
    val latRad = latitude.toRadians
    val a = sin(dLat / 2) * sin(dLat / 2) * sin(dLon / 2) * sin(dLon / 2) * cos(latRad) * cos(lat2Rad)
    val c = 2 * atan2(math.sqrt(a), math.sqrt(1 - a))
    earthRadiusMeters * c
  }
}
