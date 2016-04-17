import scala.collection.mutable.Map
import scala.io.StdIn
import scala.util.parsing.combinator._

case class CmdDelete(num: Int)
case class CmdList(name: String)
case class CmdMonthly(name: String)
case class CmdQuit()
case class CmdWeekly(name: String)

class Command extends JavaTokenParsers {
  def cmd: Parser[Any] = delete|list|monthly|quit|weekly
  def number: Parser[Int] = """\d+""".r^^(_.toInt)

  def delete: Parser[CmdDelete] = "delete"~>number^^CmdDelete
  def list: Parser[CmdList] = "list "~>"""[^\n]+""".r^^CmdList
  def monthly: Parser[CmdMonthly] = "monthly "~>"""[^\n]+""".r^^CmdMonthly
  def quit: Parser[CmdQuit] = "quit"^^(_=>CmdQuit())
  def weekly: Parser[CmdWeekly] = "weekly "~>"""[^\n]+""".r^^CmdWeekly
}

class Task(val name:String, val interval:String) {
  override def toString = name
}

object ParseCommand extends Command {
  val tasks = Map[Int,Task]()
  var nextInt = 1

  def addTask(name:String, interval:String) {
    tasks += (nextInt->new Task(name, interval))
    nextInt += 1
  }

  def run() {
    var input = " "
    while(input != "") {
      print("ht> ")
      input = StdIn.readLine()
      var p = parseAll(cmd, input)
      p match {
        case Success(CmdDelete(num), _) => tasks -= num
        case Success(CmdList("all"), _) => tasks.foreach(println)
        case Success(CmdMonthly(task), _) => addTask(task, "monthly")
        case Success(CmdQuit(), _) => return
        case Success(CmdWeekly(task), _) => addTask(task, "weekly")
        case _ => println("Unrecognised command")
      }
    }
  }
}

ParseCommand.run()