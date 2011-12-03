package com.a5.numberguesser

import util.Random


class Trainer(numbers: Int) {

  val markovChain = MarkovChain.completeChain(numbers, 1.0/numbers)
  var currentState = markovChain.selectState(Random.nextInt(numbers)+1)
  var previousState = markovChain.selectState(numbers+1)

  def guess() : Int = {
    currentState.map(_.value).getOrElse(-1)
  }

  def next() {
    currentState match {
      case Some(state) => {
        previousState = currentState
        currentState = state.selectNext()
      }
      case None => {}
    }
  }
  
  def actualValue(actualValue: Int) {
    previousState match {
      case Some(state) => {
        if (state.value != actualValue) {
          state.boostLink(actualValue, 1.5)
          state.boostLink(state.value, 0.5)
          currentState = markovChain.selectState(actualValue)
        }
      }
      case None => {}
    }
    
  }
  
}
