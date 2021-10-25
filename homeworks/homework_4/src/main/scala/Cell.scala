import scala.annotation.tailrec

trait Cell{}

class EmptyCell extends Cell{
  override def toString: String = "empty"
}

class NumberCell(val value: Int) extends Cell{
  override def toString: String = value.toString
}

class StringCell(val value: String) extends Cell{
  override def toString: String = value.toString
}

class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell{

  @tailrec
  private[this] def getContent(x: Int, y: Int, t: Table, visited : Seq[(Int, Int, Table)]): String = t.getCell(x, y).get match {
    case null => "outOfRange"
    case ref: ReferenceCell => {
      if (visited.contains((ref.ix, ref.iy, ref.table))) "cyclic"
      else getContent(ref.ix, ref.iy, ref.table, (ref.ix, ref.iy, ref.table) +: visited)
    }
    case r: Cell => r.toString
  }

  override def toString: String = getContent(ix, iy, table, Seq())
}