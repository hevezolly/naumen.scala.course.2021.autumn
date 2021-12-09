import scala.util.Try
object Exercises {

  trait Read[T] {
    def read(value: String): Either[String, T]
  }

  object Read{
    def read[T](value: String)(implicit reader: Read[T]): Either[String, T] = reader.read(value)
  }

  implicit class ReadOperations(value: String){
    def read[T](implicit reader: Read[T]): Either[String, T] = reader.read(value)
  }

  implicit def StringRead: Read[String] = (value: String) => Right(value)

  implicit def IntRead: Read[Int] = (value: String) => {
    try {
      Right(value.toInt)
    } catch {
      case e: java.lang.NumberFormatException => Left("not a number")
    }
  }

  implicit def OptRead[T: Read]: Read[Option[T]] = {
    case v if (v.startsWith("Some(") && v.endsWith(")") &&
      v.slice(5, v.length-1).read[T].isRight) => Right(Some(v.slice(5, v.length-1).read[T].right.get))
    case "None" => Right(None)
    case _ => Left("not parsable")
  }
}
