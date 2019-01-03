
class ScalaCopyTest extends FunSuite with Matchers{

  case class Employee(name:String , salary: Int)

   test("case class copy"){

     val e = Employee("emplyeeName",1230)
     val newE = e.copy(name = "newEmployeeName")

     newE.name shouldBe ("newEmployeeName")
     newE.salary shouldBe(1230)

   }

}
