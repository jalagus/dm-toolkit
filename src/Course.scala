/**
 * Created by jalagus on 14/03/15.
 */
case class Course(a: String, b: String, c: String, d:String, e:String) {
  val date:String = a.trim
  val id:String = b.trim
  val name:String = c.trim
  val credits:String = d.trim
  val grade:String = e.trim

  def getName = name
  def getId = id
  def getGrade = grade

  def isPassed = {
    if (grade == "Hyl.") false else true
  }

  def higherGradeThan(grade: Int): Boolean = {
    try {
      this.grade.toInt > grade
    } catch {
      case e:Exception => false
    }
  }

  def lowerGradeThan(grade: Int): Boolean = {
    try {
      this.grade.toInt < grade
    } catch {
      case e:Exception => false
    }
  }

  def getCsv = {
    id + ";" + date + ";" + name + ";" + credits + ";" + grade
  }
}