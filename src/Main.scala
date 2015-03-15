import scala.io.Source
import java.io._

object Main {
  def main(args: Array[String]): Unit = {
    val data = loadData("/Users/jalagus/Google Drive/Data Mining/data.txt")

    val recordsTotal = data.reduceLeft(_ ++ _).length

    println(confidence(data, List("Ohjelmoinnin perusteet"), List("Ohjelmoinnin jatkokurssi")))
  }

  def loadData(filename: String): Array[Array[Course]] = {
    val src = Source.fromFile(filename)
    val splitLines = src.getLines.map(l => l.split("\\s(?=([^\"]*\"[^\"]*\")*[^\"]*$)"))

    splitLines.map(l => l.drop(1).grouped(5).toArray.filter(r => r.length > 4)
      .map(r => Course(r(0), r(1), r(2).replace("\"", ""), r(3), r(4)))).toArray
  }


  def hasStudentPassedCourses(student: Array[Course], courselist: List[String]): Boolean = {
    listHasCourses(student.filter(_.isPassed), courselist)
  }

  def isStudentGradeBetweenInCourses(student: Array[Course], courselist: List[String], from: Int, to:Int): Boolean = {
    listHasCourses(student.filter(course => course.higherGradeThan(from) && course.lowerGradeThan(to)), courselist)
  }

  def listHasCourses(student: Array[Course], courselist: List[String]): Boolean = {
    var finallist = courselist

    student.foreach(course => {
      finallist = removeFromList(finallist, course.getName)
    })

    finallist.isEmpty
  }

  def confidence(students: Array[Array[Course]], A:List[String], B:List[String]): Double = {
    (students.count(courselist => listHasCourses(courselist, A ++ B)) * 1.0) / students.count(courselist => listHasCourses(courselist, A))
  }

  def removeFromList(l: List[String], elem: String): List[String] = {
    l diff List(elem)
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

}

