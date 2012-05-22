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

import org.joda.time.{DateTime, ReadableDateTime}

case class UserState(position: Option[Position] = None,
                     lastPositionUpdate: Option[ReadableDateTime] = None,
                     trackPositionOnMap: Boolean = true,
                     range: Option[WalkingDistance] = Some(WalkingDistance(500)), 
                     stopsToShow: Option[Int] = Some(10),
                     preferredRoute: Option[TransportRoute] = None,
                     preferredTransports: Transports = Transports()) {
  
  def at(pos: Position) = Option(pos) match {
    case None => this
    case opt => this.copy(position = opt, lastPositionUpdate = Some(new DateTime()))
  }
  
  def prefers(tt: TransportType) = preferredTransports includes tt
  
  def withTransport(tt: TransportType, on: Boolean) = 
    this.copy(preferredTransports = if (on) preferredTransports add tt else preferredTransports remove tt)
  
  def withPreferredRoute(preferredRoute: TransportRoute) = this.copy(preferredRoute = Option(preferredRoute))
  
  def withRange(distance: WalkingDistance) = this.copy(range = Option(distance))
  
  def withTrackedMap(on: Boolean) = this.copy(trackPositionOnMap = on)
  
  def withStopsToShow(count: Int) = this.copy(stopsToShow = if (count > 0) Some(count) else None)
}
