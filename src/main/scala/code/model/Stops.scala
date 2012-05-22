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

case class Stops(position: Position, 
                 stops: List[Stop], 
                 walkingDistance: Option[WalkingDistance] = None) {

  def forRoute(routeBox: Option[TransportRoute]): Stops = {
    routeBox match {
      case Some(route) => Stops(position, stops map withLinesFor(route) filter (hasLines), walkingDistance)
      case None => this
    }
  }

  private def withLinesFor(route: TransportRoute) = (stop: Stop) => stop.copy(Lines = stop.Lines filter lineFor(route))

  private def lineFor(route: TransportRoute) = (line: Line) => line.LineID == route.id

  private def hasLines = (stop: Stop) => !stop.Lines.isEmpty

  val hits = stops.size

  def scaledTo(userState: UserState) =
    Stops(position, qualifyingStops(userState), userState.range)

  def farFrom(position: Position) = !closeTo(position)

  def closeTo(position: Position) = this.position closeTo position

  private def qualifyingStops(userState: UserState): List[Stop] = {
    val sorted = stops sortBy (_.WalkingDistance)
    val pruned = userState.stopsToShow.map(sorted take _) getOrElse sorted
    userState.range.map(pruned filter withinDistance(_)) getOrElse pruned
  }

  private def withinDistance(distance: WalkingDistance): (Stop) => Boolean =
    (stop: Stop) => WalkingDistance(stop.WalkingDistance) lessThan distance
}
