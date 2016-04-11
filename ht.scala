import scala.io.StdIn
import scala.util.parsing.combinator._

class Command extends JavaTokenParsers {
  def cmd: Parser[String] = quit
  def quit: Parser[String] = "quit"
}

object ParseCommand extends Command {
  def run() {
    var input = " "
    while(input != "") {
      print("ht> ")
      input = StdIn.readLine()
      var p = parseAll(cmd, input)
      p match {
        case Success("quit", _) => return
        case _ => println("Unrecognised command")
      }
    }
  }
}

ParseCommand.run()