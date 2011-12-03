package com.a5.numberguesser

case class Link[T](from: State[T], to: State[T], var count: Double) {
  override def toString = {"Link[" + from.value + " -> " + to.value + "(" + count + ")]"}
} 

case class State[T](value: T, var links: List[Link[T]]) {
  import util.Random

  def sumLinkCounts() = links.foldLeft(0.0)((sum, link) => sum + link.count)

  def normalizeLinkCounts() {
    links.foreach((link) => {link.count /= sumLinkCounts})
  }

  def selectNext() : Option[State[T]] = {
    val random = Random.nextDouble()
    var lowerCount = 0.0
    def rangePredicate(link: Link[T]): Boolean = {
      if (random >= lowerCount && random <= link.count+lowerCount) {
        return true
      } 
      lowerCount += link.count
      false
    }
    links.find(rangePredicate) match {
      case Some(link) => Some(link.to)
      case None => Some(links.last.to)
    }
  }

  def boostLink(toValue: T, boost: Double) {
    links.find(_.to.value == toValue) match {
      case Some(link) => {
        link.count = link.count*boost
        normalizeLinkCounts()
      }
      case None => {}
    }
  }

  def weakenLinksOtherThen(otherValue: T, boost: Double) {
    links.filter(_.to.value != otherValue).foreach((link) => if (link.count > 0) link.count = link.count/boost)
    normalizeLinkCounts()
  }

}

case class MarkovChain[T](var states: List[State[T]]) {

  def selectState(value: T) = {
    states.find(_.value == value)
  }
}

object MarkovChain {

  def completeChain(size: Int, defaultCount: Double): MarkovChain[Int] = {
    val states = for (i <- 1 to size) yield State(i, List())
    for (from <- states) {
      var links : List[Link[Int]] = Nil
      for (to <- states) {
        links = Link(from, to, defaultCount) :: links 
      }
      from.links = links
      from.normalizeLinkCounts()
    }

    MarkovChain(states.toList)
  } 

}

