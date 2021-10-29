object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal


  case class Shelter[+T <: Animal](animals: List[T]){

    def +[I >: T <: Animal](animal: I): Shelter[I] = new Shelter[I](animals :+ animal)

    def ++[I >: T <: Animal](shelter: Shelter[I]): Shelter[I] = new Shelter[I](animals ++ shelter.animals)

    def getNames: Seq[String] = animals.map(a => a.name)

    def feed[O >: T <: Animal](food: Food[O]): Seq[String] = animals.map(a => food.feed(a))
  }


  trait Food[T <: Animal]{
    protected def foodName: String
    def feed[O >: T <: Animal](eater: O): String = eater.name + " eats " + foodName
  }

  case object Meat extends Food[Animal] {
    override protected def foodName: String = "meat"
  }

  case object Milk extends Food[Cat] {
    override protected def foodName: String = "milk"
  }

  case object Bread extends Food[Dog] {
    override protected def foodName: String = "bread"
  }
}
