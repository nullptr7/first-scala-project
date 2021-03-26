package homegrown.collections.testing

object ScopeDemo extends App {

  trait Vehicle {

    def vehicleName: String
    def vehicleType: String

    def create: Vehicle
  }

  object Vehicle {

    private[this] def createInstanceOfVehicle[R <: Vehicle]: R = {
      ???
    }

  }

}
