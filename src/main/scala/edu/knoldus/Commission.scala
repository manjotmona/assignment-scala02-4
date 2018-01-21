package edu.knoldus


/**
 * Created by manjot on 18/1/18.
 */
abstract class Commission(value: Int)

case class ClientSideCommission(value: Int) extends Commission(value: Int)

case class StreetSideCommission(value: Int) extends Commission(value: Int)

private sealed trait CommissionDisplay {
  def totalDisplayCommission: String
}

import scala.reflect.runtime.universe._

private class TotalCommission[T: TypeTag](GenericList: List[T]) extends CommissionDisplay {
  def totalDisplayCommission: String = {
    def internalSumFunc(commissionList: List[T],
        sum: Int, client: Boolean, street: Boolean): String = {
      commissionList match {
        case Nil => methodPrint(sum, client, street)
        case head :: tail if head.isInstanceOf[ClientSideCommission] =>
          val clientTempInstance = head.asInstanceOf[ClientSideCommission]
          internalSumFunc(tail, sum + clientTempInstance.value, true, street)
        case head :: tail if head.isInstanceOf[StreetSideCommission] =>
          val streetTempInstance = head.asInstanceOf[StreetSideCommission]
          internalSumFunc(tail, sum + streetTempInstance.value, client, true)
        case _ => "No Matching"
      }
    }
    internalSumFunc(GenericList, 0, false, false)
  }

  def methodPrint(sum: Int, client: Boolean, street: Boolean): String = {
    (client, street) match {
      case (true, false) => "The total client commission is  " + sum
      case (false, true) => "The total street commission is  " + sum
      case (true,true) => "The total mixed commission is  " + sum
      case (false,false) => "There is no commission."
    }
  }
}

private object Application extends App {
  val firstClient = new ClientSideCommission(2)
  val secondClient = new ClientSideCommission(2)
  val thirdClient = new ClientSideCommission(2)
  val firstStreet = new StreetSideCommission(2)
  val secondStreet = new StreetSideCommission(2)
  val totalCommission = new TotalCommission[Commission](List(firstClient,firstStreet,secondClient,secondStreet,thirdClient))
  print(totalCommission.totalDisplayCommission)
}




