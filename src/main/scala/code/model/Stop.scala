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


