import scala.collection.mutable.Map
import scala.io.StdIn
import scala.util.parsing.combinator._

class Command extends JavaTokenParsers {
  def cmd: Parser[Any] = delete|list|quit|weekly
  def number: Parser[String] = """\d+""".r

  def delete: Parser[Int] = "delete"~>number^^(_.toInt)
  def list: Parser[String] = "list weekly"
  def quit: Parser[String] = "quit"
  def weekly: Parser[String] = "weekly "~>"""[^\n]+""".r
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
        case Success("list weekly", _) => tasks.foreach(println)
        case Success("quit", _) => return
        case Success(num:Int, _) => tasks -= num
        case Success(task:String, _) => addTask(task, "weekly")
        case _ => println("Unrecognised command")
      }
    }
  }
}

ParseCommand.run()