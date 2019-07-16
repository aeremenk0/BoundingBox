package ru.eremenko


// v stands for vertical, h for horizontal
case class Point(v: Int, h: Int){
  def hLen(p: Point): Int = {
    p.h - h
  }
}
