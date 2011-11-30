
case class Link[T](val from: State[T], val to: State[T], var count: Int) {
  override def toString = {"Link[" + from.value + " -> " + to.value + "(" + count + ")]"}
} 

case class State[T](val value: T, var links: List[Link[T]]) {
  import scala.util.Random  

  def selectNext() : Option[State[T]] = {
    val sumLinkCount = links.foldLeft(0)((sum, link) => sum + link.count)
    val random = Random.nextInt(sumLinkCount)
    var lowerCount = 0
    def rangePredicate(link: Link[T]): Boolean = {
      if (random >= lowerCount && random <= link.count+lowerCount) {
        return true
      } 
      lowerCount += link.count
      return false
    }
    links.find(rangePredicate) match {
      case Some(link) => Some(link.to)
      case None => None
    }
  }

  def boostLink(toValue: T) {
    links.find(_.to.value == toValue) match {
      case Some(link) => {link.count = link.count+1}
      case None => {}
    }
  }

  def weakenLinksOtherThen(otherValue: T) {
    links.filter(_.to.value != otherValue).foreach((link) => if (link.count > 0) link.count = link.count-1)
  }

}

case class MarkovChain[T](var states: List[State[T]])

object MarkovChain {

  def completeChain(size: Int, defaultCount: Int): MarkovChain[Int] = {
    val states = for (i <- 1 to size) yield State(i, List())
    for (from <- states) {
      var links : List[Link[Int]] = Nil
      for (to <- states) {
        links = Link(from, to, defaultCount) :: links 
      }
      from.links = links
    }

    return MarkovChain(states.toList)
  } 

}

var state = MarkovChain.completeChain(4, 5).states(0)
def nextState[T](max: Int, state: State[Int]) {
  if (max <= 0) return 
  state.selectNext match {
    case Some(s) => {
      println(s)
      if (s.value % 2 != 0) state.weakenLinksOtherThen(s.value)
      nextState(max - 1, s)
    }
    case None => {
      println("No next state")
    } 
  }
}
println(state)
nextState(500, state)

