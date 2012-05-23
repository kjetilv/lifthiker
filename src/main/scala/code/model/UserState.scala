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
import net.liftweb.http.SessionVar

object UserState {
  
  private object userStateSessionVar extends SessionVar[UserState](UserState())
  
  def update(updater: UserState => UserState) = userStateSessionVar(updater(userStateSessionVar.get))
  
  def get = userStateSessionVar.get
}

case class UserState(position: Option[Position] = None,
                     lastPositionUpdate: Option[ReadableDateTime] = None,
                     trackPositionOnMap: Boolean = true,
                     range: Option[WalkingDistance] = Some(WalkingDistance(500)),
                     selectedStop: Option[Stop] = None,
                     stopsToShow: Option[Int] = Some(10),
                     preferredRoute: Option[TransportRoute] = None,
                     preferredTransports: Transports = Transports(Trikk, Bane, Buss)) {
  
  private val typeIds = preferredTransports.types.map(_.typeId) 
  
  def isSingleRoute = preferredRoute.isDefined 
  
  def at(pos: Option[Position]) = this.copy(position = pos, lastPositionUpdate = Some(new DateTime()))
  
  def canRideWith(typeId: Int) = typeIds contains typeId
  
  def canRideWith(tt: TransportType) = preferredTransports includes tt
  
  def withTransport(tt: TransportType, ok: Boolean) = 
    this.copy(preferredTransports = if (ok) preferredTransports add tt else preferredTransports remove tt)
  
  def withPreferredRoute(preferredRoute: Option[TransportRoute]) = this.copy(preferredRoute = preferredRoute)
  
  def withRange(distance: Option[WalkingDistance]) = this.copy(range = distance)
  
  def withTrackedMap(on: Boolean) = this.copy(trackPositionOnMap = on)
  
  def withStopsToShow(count: Option[Int]) = this.copy(stopsToShow = count.filter(_ > 0))
}
