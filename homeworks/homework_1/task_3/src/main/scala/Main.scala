object Main extends App{
  val name = "Seva Finogenov"
  var possibleGreetings: List[String] = List("Hello", "Holla", "Privet")
  val rest = "Scala! This is"

  val sayHello = (h: String, r: String, n: String) => println(h + " " + r + " " + n)

  class HelloSayer(name: String){
    def greet: Unit = {
      for (greet <- possibleGreetings) sayHello(greet, rest, name)
      println
    }
  }

  val standartSayer = new HelloSayer(name)
  standartSayer.greet

  val reverseSayer = new HelloSayer(name.reverse)
  reverseSayer.greet
}
