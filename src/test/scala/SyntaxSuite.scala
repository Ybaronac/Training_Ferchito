package co.com.scalatraining.syntax

import org.scalatest.FunSuite

class SyntaxSuite extends FunSuite{

  test("Un var debe permitir realizar asignaciones"){
    var x = 0
    assert(x == 0)
    x = 2
    assert(x == 2)
  }

  test("Un val no debe permitir realizar asignaciones"){
    val x = 0
    assert(x == 0)
    assertDoesNotCompile("x = 1")
  }

  test("Los tipos en Scala son inferidos por el compilador"){
    // Fijate como no hay que decir de qué tipo es x
    val x = 0
    assert(x == 0)

    // Aunque tambien lo puedes hacer explicito si asi lo quieres
    val y = "0"
    assert(y == "0")

    // Si eres incredulo fijate como el tipo es fuerte y no debil
    var strong = "0"

    assertDoesNotCompile ("strong = 1")
  }

  test("Scala no debe permitir iniciar en null"){
    var x: Null = null
    assertDoesNotCompile("x = 1")
  }

  test("Scala no debe permitir declarar sin asignar"){
    assertDoesNotCompile("var x")
  }

  test("Un object puede tener funciones miembro"){

    object obj {

      var x = 1
      val y = 0

      def f1(a: Int, b:Int):Int = {
        x = x + 1
        a + x
      }

      def f2(a: Int) = {
        a + 2
      }
    }

    //fijate como no hay que hacer new de obj
    val res = obj.f2(1)

    assert(res == 3)
  }

  test("Un class se puede comoportar como un class tradicional"){

    //los parametros de contruccion se definen entre parentesis a continuacion del nombre de la clase
    class MyClass(a:Int){
      def f1 = a + 1
      def f2 = a + 2

    }

    // A una class se le debe instanciar con new pasándole los atributos que define para su construccion
    val mc = new MyClass(1)

    val res = mc.f1
    assert(res == 2)
    assertDoesNotCompile("println(mc.a)")
  }


  test("A un class se le puede  mutar su estado"){

    //los parametros de contruccion se definen entre parentesis a continuacion del nombre de la clase
    class MyClass(a:Int){

      var r = 0

      def f1 = {
        r = r + 2
        a + 1
      }

      def f2 = a + 2
    }

    // A una class se le debe instanciar con new pasándole los atributos que define para su construccion
    val mc = new MyClass(1)

    println(s"mc: ${mc}")

    assert(mc.r == 0)
    val res1 = mc.f1
    assert(mc.r == 2)
    val res2 = mc.f1
    assert(mc.r == 4)

  }

  test("Un case es una clase normal para usos especificos"){

    case class MyCaseClass(var a:Int,var b:Int) {
      def f1(a:Int) = a + 1
    }

    // Se puede instanciar de forma normal
    val mcc1 = new MyCaseClass(1, 2)
    assert(mcc1.f1(1) == 2)

    // Se puede instanciar sin new
    val mcc2 = MyCaseClass(1,2)
    println(s"mcc: ${mcc2}")

    assert(mcc2.f1(1) == 2)

    //Que pasa si intentamos println(mcc2) ?
    println(mcc2)
    // Pregunta cuáles son esos casos específicos

  }

  test("Un trait puede tener solo definiciones"){
    trait MyTrait {
      def f1(a:Int):Boolean
    }

    trait MySecondTrait{
      def f2(a:String):Int
    }

    class MyClass extends MyTrait with MySecondTrait{
      override def f1(a:Int) = ???
      override def f2(a:String) = ???
    }

    assertThrows[NotImplementedError]{
      val mc = new MyClass
      mc.f1(1)
    }

  }

  test("Un trait puede tener tambien implementaciones0"){
    trait MyTrait {
      def f1(a:Int):Int = a + 1
    }

    class MyClass extends MyTrait

    val mc = new MyClass
    val res = mc.f1(1)
    assert(res == 2)
  }

  test("Un trait puede tener tambien implementaciones1"){
    trait MyTrait {
      def f1(a:Int) = a + 1
    }
    object MyObject extends MyTrait{
    }
    val res = MyObject.f1(1)

    assert(res == 2)
  }

  test("Un trait puede tener tambien implementaciones2"){
    trait MyTrait {
      val v: Int = 1
      def f1(a:Int):Int = a + 1 + v
    }


    class MyClass extends MyTrait

    val mc = new MyClass
    val res = mc.f1(1)
    assert(res == 3)
  }

  test("Un trait puede tener tambien implementaciones3"){
    trait MyTrait {
      val v: Int
      def f1(a:Int):Int = a + 1 + v
    }

    class MyClass(val v:Int = 2) extends MyTrait

    val mc = new MyClass
    val res = mc.f1(1)
    assert(res == 4)
  }

  test("Un trait puede tener tambien implementaciones4"){
    trait MyTrait {
      val v: Int
      def f1(a:Int):Int = a + 1 + v
    }

    object myObject extends MyTrait{
      val v = 2
    }


    val res = myObject.f1(1)
    assert(res == 4)
  }


  test("Pattern matching"){
    case class Profesor(nombre:String)
    case class Curso(nombre:String, p:Profesor)

    val c1 = Curso("Scala", Profesor("JP"))

    c1 match {
      case x:Curso if x.p.nombre == "JP"=> {
        assert(x.nombre=="Scala")
        assert(x.p==Profesor("JP"))
      }
      case Curso(n,p) if p.nombre == "JP" => {
        println("Pasa?")
        assert(n=="Scala")
        assert(p==Profesor("JP"))
      }
    }

  }

  test("verificando pattern machine 2"){
    class Curso(nombre:String)

    object Curso{
      def unapply(curso:Curso): Option[String] = Some("abc")
    }

    val c = new Curso("Scala")
    val c2= new Curso("Scalas")

    c match {
      case Curso(n) =>{
        assert(n=="abc")
      }
    }

    c2 match {
      case Curso(n) =>{
        assert(n=="abc")
      }
    }

  }

  test("verificando el método apply2"){
    case class MyCaseClass(a:Int, b:String)
    val mcc1 = MyCaseClass(1,"1")
    val onApply= MyCaseClass.apply(1,"1")

    assert(mcc1==onApply)
  }

  test("verificando funcionamiento de Lazy"){
    def foo (i:Int):Int={
      println( s"evaluando foo con $i")
      i
    }

    lazy val res = foo(1)
    assert(res==1)
  }
}
