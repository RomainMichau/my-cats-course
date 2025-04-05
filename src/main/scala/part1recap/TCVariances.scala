package part1recap

object TCVariances {
  import cats.Eq
  import cats.syntax.eq._
  import cats.instances.int._
  import cats.instances.option._

  val aComp = Option(2) === Option(3)
//  val anInvalidComp = Some(2) === None
  class Animal
  class Cat extends Animal

  // covariant:
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat]


  // contravariant
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal]

  trait SoundMaker[-T]
  implicit object AnimalSoundMake extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow")
  makeSound[Animal]
  makeSound[Cat]

  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // covariantTC
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animal everywhere"
  }

  implicit object CatShow extends AnimalShow[Cat] {
    override def show: String = "so many cats"
  }
   def organiseShow[T](implicit event: AnimalShow[T]): String = event.show

  def main(args: Array[String]): Unit = {
    println(organiseShow[Cat])
//    println(organiseShow[Animal])
  }

}
