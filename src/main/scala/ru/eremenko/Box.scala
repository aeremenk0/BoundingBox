package ru.eremenko

case class Box(topLeft: Point, bottomRight: Point) {
  def isOverlaps(b: Box): Boolean = {
    if (topLeft.h > b.bottomRight.h || b.topLeft.h > bottomRight.h){
      false
    } else if (bottomRight.v < b.topLeft.v || b.bottomRight.v < topLeft.v){
      false
    } else {
      true
    }
  }

  override def toString: String = s"(${topLeft.v+1},${topLeft.h+1})(${bottomRight.v+1},${bottomRight.h+1})"
//  override def toString: String = s"(${topLeft.v},${topLeft.h})(${bottomRight.v},${bottomRight.h})"
}