package fpinscala.state

import fpinscala.state.CandyMachine.{Input, Machine}
import org.scalacheck.commands.Commands
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

import scala.util.{Failure, Success, Try}

object CommandsCandyMachine extends Properties("CommandsCandyMachine") {
  property("candymachinespec") = CandyMachineSpec.property()
}

object CandyMachineSpec extends Commands {

  val genCandyMachine: Gen[Machine] =
    for {
      locked <- Arbitrary.arbBool.arbitrary
      candies <- Gen.choose(0, 20)
      coins <- Gen.choose(0, 20)
    } yield Machine(locked, candies, coins)

  case class MutableCandyMachine(machine: Machine) {
    var machineState: Machine = machine
    def run(inputs: List[Input]): Unit =
      machineState = CandyMachine.simulateMachine(inputs).run(machineState)._2
  }

  case class ModelState(locked: Boolean, candies: Int, coins: Int)

  override type State = ModelState
  override type Sut = MutableCandyMachine

  override def canCreateNewSut(newState: State,
                               initSuts: Traversable[State],
                               runningSuts: Traversable[Sut]): Boolean = {
    initSuts.isEmpty && runningSuts.isEmpty
  }

  override def newSut(state: State): Sut =
    MutableCandyMachine(Machine(state.locked, state.candies, state.coins))

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State): Boolean = true

  override def genInitialState: Gen[State] =
    for {
      machine <- genCandyMachine
    } yield ModelState(machine.locked, machine.candies, machine.coins)


  override def genCommand(state: State): Gen[Command] = Gen.oneOf(Coin, Turn)

  sealed class CandyMachineCommand(input: CandyMachine.Input) extends Command {
    override type Result = ModelState

    override def run(sut: Sut): Result = {
      sut.run(List(input))
      ModelState(sut.machineState.locked, sut.machineState.candies, sut.machineState.coins)
    }

    override def nextState(state: State): State = {
      val s = CandyMachine
        .simulateMachine(List(input))
        .run(Machine(state.locked, state.candies, state.coins))
      val m = s._2
      ModelState(m.locked, m.candies, m.coins)
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop =
      result match {
        case Success(resultState) =>
          val currState = CandyMachine
            .simulateMachine(List(input))
            .run(Machine(state.locked, state.candies, state.coins))
          val currMachine = currState._2
          val modelState = ModelState(currMachine.locked,
                                      currMachine.candies,
                                      currMachine.coins)
          modelState == resultState

        case Failure(_) => false
      }
  }

  case object Coin extends CandyMachineCommand(CandyMachine.Coin)

  case object Turn extends CandyMachineCommand(CandyMachine.Turn)

}
