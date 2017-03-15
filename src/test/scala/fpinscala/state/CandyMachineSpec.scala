package fpinscala.state

import fpinscala._
import fpinscala.state.CandyMachine.Machine
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop, Properties}

import scala.util.{Failure, Success, Try}

object CommandsCandyMachine extends Properties("CommandsCandyMachine") {
  property("candymachinespec") = CandyMachineSpec.property()
}

object CandyMachineSpec extends Commands {
  override type State = Machine
  override type Sut = Machine

  override def canCreateNewSut(newState: State,
                               initSuts: Traversable[State],
                               runningSuts: Traversable[Sut]): Boolean = {
    initSuts.isEmpty && runningSuts.isEmpty
  }

  override def newSut(state: State): Sut =
    Machine(state.locked, state.candies, state.coins)
  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State): Boolean = state.locked

  override def genInitialState: Gen[State] =
    Machine(locked = true, candies = 10, coins = 0)

  override def genCommand(state: State): Gen[Command] = Gen.oneOf(Coin, Turn)

  case object Coin extends Command {
    override type Result = state.State[Machine, (Int, Int)]

    override def run(sut: Sut): Result =
      CandyMachine.simulateMachine(List(CandyMachine.Coin))

    override def nextState(state: State): State =
      CandyMachine.simulateMachine(List(CandyMachine.Coin)).run(state)._2

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop =
      result match {
        case Success(s) =>
          s.run(state)._2 == CandyMachine
            .simulateMachine(List(CandyMachine.Coin))
            .run(state)
            ._2
        case Failure(_) => false
      }
  }

  case object Turn extends Command {
    override type Result = state.State[Machine, (Int, Int)]

    override def run(sut: Sut): Result =
      CandyMachine.simulateMachine(List(CandyMachine.Turn))

    override def nextState(state: State): State =
      CandyMachine.simulateMachine(List(CandyMachine.Turn)).run(state)._2

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop =
      result match {
        case Success(s) =>
          s.run(state)._2 == CandyMachine
            .simulateMachine(List(CandyMachine.Turn))
            .run(state)
            ._2
        case Failure(_) => false
      }
  }
}
