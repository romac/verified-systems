import stainless.lang._
import stainless.collection._

object asm {

  sealed abstract class Reg
  final case object Rax extends Reg
  final case object Rbx extends Reg

  sealed abstract class Instr
  final case class Mov(to: Reg, from: Reg)     extends Instr
  final case class Movi(to: Reg, value: Int)   extends Instr
  final case class Add(dest: Reg, source: Reg) extends Instr
  final case class Addi(dest: Reg, value: Int) extends Instr

  case class State(reg: Map[Reg, Int]) {
    require((reg contains Rax) && (reg contains Rbx))

    def apply(instr: Instr): State = instr match {
      case Mov(to, from)   => update (to -> reg(from))
      case Movi(to, value) => update (to -> value)
      case Add(to, from)   => update (to -> (reg(to) + reg(from)))
      case Addi(to, value) => update (to -> (reg(to) + value))
    }

    def update(rv: (Reg, Int)): State = {
      State(reg.updated(rv._1, rv._2))
    }

    def exec(instrs: List[Instr]): State = instrs match {
      case Nil()               => State(reg)
      case Cons(instr, instrs) => apply(instr).exec(instrs)
    }
  }

  val prog: List[Instr] = List(
    Addi(Rax, 2),  // rax = rax + 2
    Add(Rax, Rbx), // rax = rax + rbx
    Mov(Rbx, Rax), // rbx = rax
    Addi(Rbx, 10)  // rbx = rbx + 10
  )

  // val init = State(Map(Rax -> 0, Rbx -> 0))

  def test(init: State) = {
    init.exec(prog) == init
  } holds

  // Stainless yields: State(Map(Rax -> 0, Rbx -> 10))
  // def solution: State = {
  //   choose { (state: State) =>
  //     state.exec(prog).reg(Rbx) == 22
  //   }
  // }
}

