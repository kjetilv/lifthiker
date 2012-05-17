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
