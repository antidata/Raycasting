
import scala.collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: antidata
 * Date: 11/2/13
 * Time: 5:20 PM
 * To change this template use File | Settings | File Templates.
 */


object RcDefinitions {

  abstract class Parameter[T](val value : T, group : ParameterType)
  abstract class ParameterType(val group : String)
  implicit def par2value[T](par : Parameter[T]) : T = par.value
  implicit def double2float(d : Double) : Float = d.toFloat
  case object Size extends ParameterType("size")
  case object Angle extends ParameterType("angle")
  case object MapSpec extends ParameterType("map spec")
  case object MiniMap extends ParameterType("Mini Map")

  // size of wall height
  case object TileSize extends Parameter(64, Size)
  case object WallHeight extends Parameter(64, Size)
  case object ProjectionPlaneWidth extends Parameter(320, Size)
  case object ProjectionPlaneHeight extends Parameter(200, Size)
  case object Angle60 extends Parameter(ProjectionPlaneWidth.value, Angle)
  case object Angle30 extends Parameter(Angle60 / 2, Angle)
  case object Angle15 extends Parameter(Angle30 / 2, Angle)
  case object Angle90 extends Parameter(Angle30 * 3, Angle)
  case object Angle180 extends Parameter(Angle90 * 2, Angle)
  case object Angle270 extends Parameter(Angle90 * 3, Angle)
  case object Angle360 extends Parameter(Angle60 * 6, Angle)
  case object Angle0 extends Parameter(0, Angle)
  case object Angle5 extends Parameter(Angle30 / 6, Angle)
  case object Angle10 extends Parameter(Angle5 * 2, Angle)

  case object Wall extends Parameter(1.toByte, MapSpec)
  case object Empty extends Parameter(0.toByte, MapSpec)

  case object MapWidth extends Parameter(12, MiniMap)
  case object MapHeight extends Parameter(12, MiniMap)

  def arcToRad(arcAngle : Float) : Float = (arcAngle * Math.PI) / Angle180

  case class DataTable(Sin : ArrayBuffer[Float], ISin : ArrayBuffer[Float], Cos : ArrayBuffer[Float], ICos : ArrayBuffer[Float],
                        Tan : ArrayBuffer[Float], ITan : ArrayBuffer[Float], Fish : ArrayBuffer[Float],
                        xStep : ArrayBuffer[Float], yStep : ArrayBuffer[Float])

  lazy val TrigonometricTables = {
    val sin = ArrayBuffer[Float]((0 to (Angle360)).map(i => 0F):_*)
    val iSin = ArrayBuffer[Float]((0 to (Angle360)).map(i =>0F):_*)
    val cos = ArrayBuffer[Float]((0 to (Angle360)).map(i =>0F):_*)
    val iCos = ArrayBuffer[Float]((0 to (Angle360)).map(i =>0F):_*)
    val tan = ArrayBuffer[Float]((0 to (Angle360)).map(i =>0F):_*)
    val iTan = ArrayBuffer[Float]((0 to (Angle360)).map(i =>0F):_*)
    val fish = ArrayBuffer[Float]((0 to (Angle360)).map(i =>0F):_*)
    val xStep = ArrayBuffer[Float]((0 to (Angle360)).map(i =>0F):_*)
    val yStep = ArrayBuffer[Float]((0 to (Angle360)).map(i =>0F):_*)

    def setValuesFor(i : Int) {
      val radian : Float = arcToRad(i) + 0.0001F
      sin.update(i, Math.sin(radian))
      iSin.update(i, (1.0F / sin(i)))
      cos.update(i, Math.cos(radian))
      iCos.update(i, (1.0F / cos(i)))
      tan.update(i, Math.tan(radian))
      iTan.update(i, (1.0F / tan(i)))
      
      if(i >= Angle90 && i < Angle270) {//facing left
    	  xStep.update(i, (TileSize / tan(i)))
    	  if(xStep(i)>0) xStep.update(i, -1 * xStep(i))
      } else {//facing right
		  xStep.update(i, (TileSize / tan(i)))
		  if(xStep(i)<0) xStep.update(i, -1 * xStep(i))
      }
      if(i >= Angle0 && i < Angle180) {// facing down
    	  yStep.update(i, (TileSize * tan(i)))
    	  if(yStep(i)<0) yStep.update(i, -1 * yStep(i))
      } else {//facing up
        yStep.update(i, (TileSize * tan(i)))
        if(yStep(i)>0) yStep.update(i, -1 * yStep(i))
      }
    }

    (0 to Angle360) map setValuesFor

    def setFish(i : Int) {
      val radian = arcToRad(i)
      fish.update(i + Angle30, (1.0F / Math.cos(radian)))
    }
    ((-1 * Angle30) to Angle30) map setFish

    DataTable(sin,iSin,cos,iCos,tan,iTan,fish,xStep,yStep)
  }

  val Map = Array[Byte](
    Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,
    Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,
    Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,
    Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty,Empty,Wall,
    Wall,Empty,Empty,Wall,Empty,Wall,Empty,Empty,Wall,Empty,Empty,Wall,
    Wall,Empty,Empty,Wall,Empty,Wall,Wall,Empty,Wall,Empty,Empty,Wall,
    Wall,Empty,Empty,Wall,Empty,Empty,Wall,Empty,Wall,Empty,Empty,Wall,
    Wall,Empty,Empty,Empty,Wall,Empty,Wall,Empty,Wall,Empty,Empty,Wall,
    Wall,Empty,Empty,Empty,Wall,Empty,Wall,Empty,Wall,Empty,Empty,Wall,
    Wall,Empty,Empty,Empty,Wall,Wall,Wall,Empty,Wall,Empty,Empty,Wall,
    Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,
    Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall
  )
  // player's attributes
  val playerX = 100
  val playerY = 160
  val playerArc = Angle0
  val playerDistToProjectionPlane = 277
  val playerHeight = 32
  val playerSpeed = 16
  val projectionPlaneYCenter = ProjectionPlaneHeight / 2
  val playerMapX = 0
  val playerMapY = 0
  val miniMapWidth = 0
}
