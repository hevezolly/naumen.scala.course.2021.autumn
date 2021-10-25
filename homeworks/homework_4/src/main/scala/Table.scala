import scala.collection.mutable.ArrayBuffer

class Table(val width: Int, val height: Int){
  private val table: ArrayBuffer[Cell] = ArrayBuffer.fill[Cell](width * height){new EmptyCell()}

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix < 0 || ix >= width || iy < 0 || iy >= height) None
    else Some(table(ix + iy * width))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height)
      table(ix + iy * width) = cell
  }
}