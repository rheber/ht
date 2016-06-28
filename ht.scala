import scala.collection.mutable.Map
import scala.io._
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import java.io.FileWriter

case class CmdDelete(num: Int)
case class CmdList(name: String)
case class CmdMonthly(name: String)
case class CmdQuit()
case class CmdSafeQuit()
case class CmdRenumber()
case class CmdSave()
case class CmdWeekly(name: String)

class Command extends JavaTokenParsers {
  def cmd: Parser[Any] = delete|
                         list|
                         monthly|
                         quit|
                         safequit|
                         renumber|
                         save|
                         weekly
  def number: Parser[Int] = """\d+""".r^^(_.toInt)

  def delete: Parser[CmdDelete] = "delete"~>number^^CmdDelete
  def list: Parser[CmdList] = "list "~>"""[^\n]+""".r^^CmdList
  def monthly: Parser[CmdMonthly] = "monthly "~>"""[^\n]+""".r^^CmdMonthly
  def quit: Parser[CmdQuit] = "quit!"^^(_=>CmdQuit())
  def safequit: Parser[CmdSafeQuit] = "quit"^^(_=>CmdSafeQuit())
  def renumber: Parser[CmdRenumber] = "renumber"^^(_=>CmdRenumber())
  def save: Parser[CmdSave] = "save"^^(_=>CmdSave())
  def weekly: Parser[CmdWeekly] = "weekly "~>"""[^\n]+""".r^^CmdWeekly
}

class Task(val name:String, val interval:String) {
  override def toString = name
}

object ParseCommand extends Command {
  var tasks = Map[Int,Task]()
  var modified = false
  var nextInt = 1

  def addTask(name:String, interval:String, tasks:Map[Int, Task]) {
    tasks += (nextInt->new Task(name, interval))
    nextInt += 1
    modified = true
  }

  def deleteTask(num:Int) {
    tasks -= num
    modified = true
  }

  def printlnTask(task:Tuple2[Int, Task]) {
    printf("%3d [ ] %s\n", task._1, task._2)
  }

  def renumberTasks() {
    val newTasks = Map[Int, Task]()
    nextInt = 1
    tasks.foreach(x=>addTask(x._2.name, x._2.interval, newTasks))
    tasks = newTasks
  }

  def loadTasks() {
    def loadTask(line:String) {
      val habit = "(.+)\t(.+)".r
      line match {
        case habit(name, interval) => addTask(name, interval, tasks)
      }
    }

    try {
      val f = Source.fromFile("./habits")
      f.getLines().foreach(line=>loadTask(line))
    } catch {
      case _ : Throwable => ; // no saved habits
    }
  }

  def safeQuit() {
    if(!modified) { System.exit(0) }
    println("Unsaved changes, either 'save' or 'quit!'")
  }

  def saveTasks() {
    val f = new FileWriter("./habits")
    tasks.foreach(x=>f.write("%s\t%s\n".format(x._2.name, x._2.interval)))
    f.close()
    modified = false
  }

  def run() {
    var input = " "
    loadTasks()
    modified = false // because loadTasks calls addTasks
    while(input != "") {
      print("ht> ")
      input = StdIn.readLine()
      var p = parseAll(cmd, input)
      p match {
        case Success(CmdDelete(num), _) => deleteTask(num)
        case Success(CmdList("all"), _) => tasks.foreach(printlnTask)
        case Success(CmdList(itvl), _) =>
            tasks.filter(_._2.interval == itvl).foreach(printlnTask)
        case Success(CmdMonthly(task), _) => addTask(task, "monthly", tasks)
        case Success(CmdQuit(), _) => return
        case Success(CmdSafeQuit(), _) => safeQuit()
        case Success(CmdRenumber(), _) => renumberTasks()
        case Success(CmdSave(), _) => saveTasks()
        case Success(CmdWeekly(task), _) => addTask(task, "weekly", tasks)
        case _ => println("Unrecognised/incomplete command")
      }
    }
  }
}

ParseCommand.run()