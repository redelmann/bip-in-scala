
import language.implicitConversions

package object bip {
  
  /** Sends a useless value on the port. */
  implicit def port2awaitCase[D](port: Port[D, Unit]): AwaitCase[D, Unit, D] =
    AwaitCase(port, (), (x: D) => x)
}