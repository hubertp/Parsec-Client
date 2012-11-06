package parsecClient

import java.io._
import java.lang.Thread
import java.lang.reflect
import java.lang.reflect.InvocationTargetException
import java.awt.GraphicsConfiguration
import swing._
import scala.util.parsing.combinator.debugging.AndOrZipper
import scala.util.parsing.combinator.debugging.Controllers
import scala.util.parsing.combinator.debugging
import scala.tools.nsc
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.reflect.io.{PlainDirectory, Directory, PlainFile}
import scala.collection.mutable

object Client extends SimpleSwingApplication {
  val c = new Client
  val compile = Button("Compile") { c.init; }
  val step = Button("Step") { val z = c.step; set(z) }
  val stepBack = Button("Step Back") { val z = c.stepBack; set(z) }

  val buttons = new FlowPanel(compile, step, stepBack)
  val text = new ListView(List("Nothing to display yet"))
  val topBox = new BoxPanel(Orientation.Vertical) {
    contents += buttons
    contents += text
  }

  def set(z : AndOrZipper) {
    text.listData = z.toString.split('\n')
    text.repaint
  }

  def top = {
    // Disable step and step-back
    step.enabled_=(false)
    stepBack.enabled_=(false)
    new MainFrame(null) {
      title = "Parsec Debugger"
      //contents = new FlowPanel(compile, step, stepBack)
      contents = topBox
    }
  }
}

class Client extends Controllers {

  class MainListener extends debugging.Listener {
    def stepIn(id: Int, name: String, location: debugging.ParserLocation, tree: debugging.AndOrZipper): Option[debugging.Notification] =  {
      val ack = new debugging.Notification
      lock.synchronized {
        assert(localController.isEmpty, "local controller has to be empty")
        println("[listener] adding a step: " + name)
        zs = tree :: zs
        localController = Some(ack) // this should be under synchronized
        ids.push(id)
        Some(ack) // atm we are interested in every step
      }
    }
    def stepOut(id: Int, last: Boolean): Option[debugging.Notification] = lock.synchronized {
      if (ids.nonEmpty && ids.head == id) ids.pop() else {
        if (ids.contains(id)) println("[listener] missed some parser : " + id)
        else                  println("[listener] exit parser that we never entered: " + id)
      }
      if (last && ids.isEmpty) Client.step.enabled = false
      None // atm we are not interested 
    }
  }

  val controller = new Controller // this will serve as our way of communicating with the running debugger session
  val req = new Request
  var zs : List[AndOrZipper] = Nil
  var index : Int = 0
  val methHandler = null
  var op : Thread = null
  var reporter: Reporter = null
  var listener = new MainListener()
  var localController: Option[debugging.Notification] = None
  val lock = new AnyRef()
  val ids = new mutable.Stack[Int]()

  def isParsecDone = lock.synchronized {
    ids.isEmpty
  }

  def compile : List[String] = {

    def createCompiler(out: String): (nsc.Global, nsc.Settings) = {
      val settings = new nsc.Settings()
      val props = new java.util.Properties()
      System.setProperty("parser.combinators.debug", "true") // enable macro
      System.setProperty("parser.debug", "true")             // enable standard debuging info
      props.load(new java.io.FileInputStream("local.properties"))
      val classPath = props.getProperty("scala.home") + "/lib/scala-library.jar"
      settings.classpath.value = classPath //System.getProperty("java.class.path")
      val compDirectory = new PlainDirectory(new Directory(new java.io.File(out)))
      settings.outputDirs.setSingleOutput(compDirectory)
      reporter = new ConsoleReporter(settings)
      val global = new nsc.Global(settings, reporter)
      (global, settings)
    }

    def doCompile(filesToCompile : List[String], dest : String) {
      println("[compiling]: " + filesToCompile.mkString(", "))
      val (comp, settings) = createCompiler(dest)
      val command = new nsc.CompilerCommand(filesToCompile, settings)
      val run = new comp.Run
      run compile command.files
    }

    // Get file handle of original file or directory
    val dir = "resources"
    val build = "build"
    val orig = new File(dir)
    var error : Option[String] = None
    var files : List[File] = Nil
    var fnames : List[String] = Nil
    var fpaths : List[String] = Nil

    // In case it's a directory, let the file array contain all the files of the directory
    if (orig.isDirectory) {
      files     = orig.listFiles.filter(f => """.*\.scala$""".r.findFirstIn(f.getName).isDefined).toList
      fnames    = files.map(f => f.getName)
      fpaths    = fnames.map(f => dir + "/" + f)
    }

    // Then compile the files
    doCompile(fpaths, build)
    return fnames
  }

  def init : Unit = {
    val props = new java.util.Properties()
    props.load(new java.io.FileInputStream("local.properties"))
    val x = props.getProperty("scala.home")
    //IO.load(props, f / "local.properties")
    //val x = props.getProperty("scala.home")

    // Compile files
    val files = compile // Echoed out to save a bit of time
    if (reporter.hasErrors) {
      println("Compilation failed")
    } else {
      println("Compilation succeeded")

      // Now find the class containing the main function
      val classToRun = findClass

      println("[parsec] run class " + classToRun.getName)

      // Create a controller
      controller.request = req

      // Invoke the class we found, calling run with a newly created controller
      val methHandler = classToRun.getMethod("runMain", classOf[Controller]) // runTest would be defined in Parsers and would add Controller argument to the list of listeners
      val f           = classToRun.getField("MODULE$")
      val listenerHandler = classToRun.getMethod("addListener", classOf[debugging.Listener])

      f.setAccessible(true)
      val c           = f.get(null)
      op              = new Thread() {
        override def run() {
          try {
            listenerHandler.invoke(c, listener)
            methHandler.invoke(c, controller)
          }
          catch { case e => e.getCause().printStackTrace(); }
        } 
      }

      // Enable clicking next
      Client.step.enabled = true
      Client.compile.enabled = false

      op.start()
      //testLoop(op, controller)
    }
  }

  def step : AndOrZipper = {
    if (index == 0) {
      // If the index is 0, then we need to get the next element
      next
      // if this was the last, then disable next
      if (isParsecDone) Client.step.enabled_=(false)
    } 
    else {
      index = index - 1

      // If we are moving to the end and it's last, disable next
      if (isParsecDone && index == 0) Client.step.enabled_=(false)
    }
    Client.stepBack.enabled_=(true)
    zs.drop(index).head
  }

  def stepBack : AndOrZipper = {

    if ((index + 1) == zs.length) {
      // If the index is equal to the length of the list, then we can't go any futher back
      // Should call to textfield and change it to "Already at first item"
    }
    else {
      index = index + 1
      // If index is bigger than length - 1, then we don't change the index
      // Should call to textfield and change it to "Already at first item"

      // if we are now at the first then disable prev
      if (index + 1 == zs.length) Client.stepBack.enabled_=(false)
    }
    Client.step.enabled_=(true)
    zs.drop(index).head
  }


  def next : Unit = {
    /*controller.synchronized {
      controller.notify
    }
    if (op.getState != java.lang.Thread.State.TERMINATED) {
      controller.request.synchronized {
        while (controller.request.field == null) controller.request.wait
        // Now that we are back, get the zipper and reset the controller
        zs = controller.request.field :: zs
        isDone = controller.request.isDone
        controller.request.field = null
      }
    }*/

    lock.synchronized {
      while (localController.isEmpty) lock.wait()
      // zs was filled
      val ack = localController.get
      ack.synchronized {
        ack.setReady()
        ack.notify()
      }
      localController = None
    }
  }


  def testLoop(op : Thread, c : Controller) : Unit = {
    println("testLoop: enter")
    c.synchronized {
      c.notify
    }
    if (op.getState != java.lang.Thread.State.TERMINATED) {
      c.request.synchronized {
        while (c.request.field == null) c.request.wait
        // Now that we are back, get the zipper and reset the controller
        zs = c.request.field :: zs
        c.request.field = null
      }


      // print out the zipper
      println(zs.head.toString)

      // Check if we should loop around
      println("\n>> ")
      scala.Console.readChar match {
        case 'q'    => op.stop
        case 's'    => testLoop(op, c)
        case _      => println("Press q to quit"); testLoop(op, c)
      }
    }
  }


  def findClass : Class[_] = {
    def findClass0(dir : File) : List[Class[_]] = {
      if (dir.isDirectory) {
        val classPointers = dir.listFiles.filter(f => """.*\.class$""".r.findFirstIn(f.getName).isDefined).toList
        val directories = dir.listFiles.filter(f => f.isDirectory).toList
        val classStrings = classPointers.map(c => c.getPath.split('.').head.split('/').drop(1).mkString("."))
        val classes = (for (c <- classStrings if c.last == '$') yield Class.forName(c)).filter(hasRun(_))
        return classes ++ directories.flatMap(findClass0)
      }
      else throw new Exception(dir + " is not a directory")
    }

    def hasRun(c : Class[_]) : Boolean = {
      (c.getDeclaredMethods.filter(m => m.getName == "runMain").length == 1)
    }

    val cs = findClass0(new File("build"))
    println(cs)
    cs match {
      case head::_   => head
      case _            => throw new Exception("No runDebug class in uploaded files")
    }
  }
}
