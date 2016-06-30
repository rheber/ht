import scala.collection.mutable.Map
import scala.io._
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import java.io.FileWriter
import java.util.Date

val MS_PER_DAY:Long = 1000*60*60*24

case class CmdDelete(num: Int)
case class CmdHelp()
case class CmdList(name: String)
case class CmdMark(num: Int)
case class CmdMonthly(name: String)
case class CmdQuit()
case class CmdSafeQuit()
case class CmdRenumber()
case class CmdSave()
case class CmdWeekly(name: String)

class Command extends JavaTokenParsers {
  def cmd: Parser[Any] = delete|
                         help|
                         list|
                         mark|
                         monthly|
                         quit|
                         safequit|
                         renumber|
                         save|
                         weekly
  def number: Parser[Int] = """\d+""".r^^(_.toInt)

  def delete: Parser[CmdDelete] = "delete"~>number^^CmdDelete
  def help: Parser[CmdHelp] = "help"^^(_=>CmdHelp())
  def list: Parser[CmdList] = "list "~>"""[^\n]+""".r^^CmdList
  def mark: Parser[CmdMark] = "mark"~>number^^CmdMark
  def monthly: Parser[CmdMonthly] = "monthly "~>"""[^\n]+""".r^^CmdMonthly
  def quit: Parser[CmdQuit] = "quit!"^^(_=>CmdQuit())
  def safequit: Parser[CmdSafeQuit] = "quit"^^(_=>CmdSafeQuit())
  def renumber: Parser[CmdRenumber] = "renumber"^^(_=>CmdRenumber())
  def save: Parser[CmdSave] = "save"^^(_=>CmdSave())
  def weekly: Parser[CmdWeekly] = "weekly "~>"""[^\n]+""".r^^CmdWeekly
}

class Task(val name:String, val interval:String, var date:Date) {
  override def toString = name
  def this(name:String, interval:String) = this(name, interval, new Date())
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

  def addTask(name:String, interval:String, date:String, tasks:Map[Int, Task]) {
    tasks += (nextInt->new Task(name, interval, new Date(date.toLong)))
    nextInt += 1
    modified = true
  }

  def announcements() {
    tasks.foreach(x=>if(isUrgent(x._2)) printf("Outstanding task: %s\n", x._2))
  }

  def deleteTask(num:Int) {
    tasks -= num
    modified = true
  }

  def helpMessage() {
    println()
    println("Commands:")
    println("delete <task number>\tDelete task")
    println("help\t\t\tDisplay this message")
    println("list all/monthly/weekly\tList tasks in specified category")
    println("mark <task number>\tMark task as having been done recently")
    println("monthly <task name>\tAdd new monthly task")
    println("quit\t\t\tQuit if there are no unsaved changes")
    println("quit!\t\t\tQuit without saving")
    println("renumber\t\tReset task numbering (happens automatically on exit)")
    println("save\t\t\tSave any changes")
    println("weekly <task name>\tAdd new weekly task")
    println()
  }

  def markTask(num:Int) {
    var task = tasks(num)
    val today = new Date().getTime()
    if(task.interval=="weekly") {
      task.date = new Date(7*MS_PER_DAY+today)
    } else if(task.interval=="monthly") {
      task.date = new Date(30*MS_PER_DAY+today)
    }
    modified = true
  }

  def isMarked(task:Task):Boolean = {
    val today = new Date().getTime()
    return task.date.getTime() > today
  }

  def isUrgent(task:Task):Boolean = {
    val now = new Date().getTime()
    if(task.interval=="weekly") {
      return 7*MS_PER_DAY+task.date.getTime() < now
    } else {
      return 30*MS_PER_DAY+task.date.getTime() < now
    }
  }

  def printlnTask(task:Tuple2[Int, Task]) {
    printf("%3d [%s] %s\n", task._1, if(isMarked(task._2)) "X" else " ", task._2)
  }

  def renumberTasks() {
    val newTasks = Map[Int, Task]()
    nextInt = 1
    tasks.foreach(x=>addTask(x._2.name, x._2.interval, newTasks))
    tasks = newTasks
  }

  def loadTasks() {
    def loadTask(line:String) {
      val habit = "(.+)\t(.+)\t(.+)".r
      line match {
        case habit(name, interval, date) => addTask(name, interval, date, tasks)
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
    tasks.foreach(x=>f.write(
        "%s\t%s\t%d\n".format(x._2.name, x._2.interval, x._2.date.getTime())))
    f.close()
    modified = false
  }

  def run() {
    var input = " "
    loadTasks()
    modified = false // because loadTasks calls addTasks
    announcements()
    while(input != "") {
      print("ht> ")
      input = StdIn.readLine()
      var p = parseAll(cmd, input)
      p match {
        case Success(CmdDelete(num), _) => deleteTask(num)
        case Success(CmdHelp(), _) => helpMessage()
        case Success(CmdList("all"), _) => tasks.foreach(printlnTask)
        case Success(CmdList(itvl), _) =>
            tasks.filter(_._2.interval == itvl).foreach(printlnTask)
        case Success(CmdMark(num), _) => markTask(num)
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