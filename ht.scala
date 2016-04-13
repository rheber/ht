import scala.io.StdIn
import scala.util.parsing.combinator._

class Command extends JavaTokenParsers {
  def cmd: Parser[String] = list|quit|weekly
  def list: Parser[String] = "list weekly"
  def quit: Parser[String] = "quit"
  def weekly: Parser[String] = "weekly "~>"""[^\n]+""".r
}

object ParseCommand extends Command {
  def run() {
    var weekly = List[String]()

    var input = " "
    while(input != "") {
      print("ht> ")
      input = StdIn.readLine()
      var p = parseAll(cmd, input)
      p match {
        case Success("list weekly", _) => weekly.foreach(println)
        case Success("quit", _) => return
        case Success(task, _) => weekly = task::weekly
        case _ => println("Unrecognised command")
      }
    }
  }
}

ParseCommand.run()