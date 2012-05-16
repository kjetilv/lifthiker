package code.model

case class WalkingDistance(meters: Int) {
  
  def times(factor: Int) = WalkingDistance(meters * factor)

  def lessThan(distance: WalkingDistance) = meters < distance.meters;
}
