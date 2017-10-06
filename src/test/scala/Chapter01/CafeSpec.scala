package Chapter01

import org.scalatest.{FlatSpec, Matchers}

class CafeSpec extends FlatSpec with Matchers {

  "Cafe" should "return a charge and a cup of coffee" in {
    val myCafe = new Cafe
    val myCard = new CreditCard

    val result = myCafe.buyCoffee(myCard)

    result._1 shouldBe a[Coffee]
    result._2 shouldBe a[Charge]
    result._2.amount should equal(1.5)
  }

  "Cafe" should "return a charge and many cups of coffee" in {
    val myCafe = new Cafe
    val myCard = new CreditCard

    val result = myCafe.buyCoffees(myCard, 3)

    result._1 shouldBe a[List[_]]
    result._1.size should equal(3)
    result._1.head shouldBe a[Coffee]
    result._2 shouldBe a[Charge]
    result._2.amount should equal(4.5)
  }

  "Cafe" should "coalesce charges" in {
    val myCard = new CreditCard()
    val charge1 = Charge(myCard, 3.4)
    val charge2 = Charge(myCard, 1.3)
    val charge3 = Charge(myCard, 2.3)
    val charges = List(charge1, charge2, charge3)

    val myCafe = new Cafe

    val result = myCafe.coalesce(charges)

    result shouldBe a[List[_]]
    result.size should equal(1)
    result.head shouldBe a[Charge]
    result.head.amount should equal(7.0)
  }
}
