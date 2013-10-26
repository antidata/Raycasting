
/**
 * Created with IntelliJ IDEA.
 * User: antidata
 * Date: 11/2/13
 * Time: 12:16 AM
 * To change this template use File | Settings | File Templates.
 */
import RcDefinitions._
import java.awt._
import scala.Range
import java.applet.Applet

class RayCasting extends Applet with Runnable {
  var renderT : Thread = null

  //Player variables, get default values from definition
  var px = playerX
  var py = playerY
  var parc = playerArc.value
  var pDistToProjPlane = playerDistToProjectionPlane
  var pHeight = playerHeight
  var pSpeed = playerSpeed
  var projPlaneYCenter = projectionPlaneYCenter
  var pMapX = playerMapX
  var pMapY = playerMapY
  var pMiniMapWith = 0
  var vkeyUp = false
  var vkeyDown = false
  var vkeyLeft = false
  var vkeyRight = false

  var map = Map

  var offscreenGraphics : Graphics = null
  var offscreenImg : Image = null
  override def start() {
    renderT = new Thread(this)
    renderT start()
  }

  def run() {
    requestFocus()
    offscreenImg = createImage(size().width, size().height)
    offscreenGraphics = offscreenImg.getGraphics()

    while(true) {
      if(vkeyLeft) {
        parc = parc - Angle5
        if(parc < Angle0) parc = parc + Angle360
      } else if(vkeyRight) {
        parc = parc + Angle5
        if(parc >= Angle360) parc = parc - Angle360
      }
      val playerXdir = TrigonometricTables.Cos(parc)
      val playerYdir = TrigonometricTables.Sin(parc)

      // if move backward
      if(vkeyDown) {
        px = px - (playerXdir * pSpeed)
        py = py - (playerYdir * pSpeed)
      } else if(vkeyUp) { // if move forward
        px = px + (playerXdir * pSpeed)
        py = py + (playerYdir * pSpeed)
      }
      render()
      try {
        Thread.sleep(33)
      } catch {
        case e : Exception => println(e.getMessage)
      }
    }
  }

  def drawBackground() {
    // sky
    var c = 25
    //(0 until (ProjectionPlaneHeight.value / 2) step 10)
    var r = 0
    Range(0, (ProjectionPlaneHeight / 2), 10) map {s =>
      offscreenGraphics setColor(new Color(c, 125, 225))
      offscreenGraphics fillRect(0, s, ProjectionPlaneWidth, 10)
      c = c + 20
      r = s
    }
    // floor
    var f = 22
    //(0 until ProjectionPlaneHeight step 15)
    Range(r, ProjectionPlaneHeight, 15) map {s =>
      offscreenGraphics setColor(new Color(f, 20, 20))
      offscreenGraphics fillRect(0, s, ProjectionPlaneWidth, 15)
      f = f + 15
    }
  }

  def drawMiniMap() {
    pMiniMapWith = 5
    (0 to MapWidth - 1) map {h =>
      (0 to MapHeight - 1) map { a =>
        map(a*MapWidth+h) match {
          case Wall.value => offscreenGraphics setColor Color.CYAN
          case _ => offscreenGraphics setColor Color.BLACK
        }
        offscreenGraphics fillRect(ProjectionPlaneWidth+(h*pMiniMapWith), a * pMiniMapWith, pMiniMapWith, pMiniMapWith)
      }
    }
    pMapX = ProjectionPlaneWidth + (px / TileSize * pMiniMapWith)
    pMapY = py / TileSize * pMiniMapWith
  }

  def drawDirectionOnMiniMap(x : Float, y : Float) {
    //println("x " + x.toString + " y " + y.toString)
    offscreenGraphics.setColor(Color.yellow)
    offscreenGraphics.drawLine(pMapX, pMapY, ProjectionPlaneWidth+(x*pMiniMapWith)/TileSize, y*pMiniMapWith/TileSize)
    offscreenGraphics.setColor(Color.red)
    offscreenGraphics.drawLine(pMapX, pMapY, pMapX+TrigonometricTables.Cos(parc)*10,pMapY+TrigonometricTables.Sin(parc)*10)
  }

  def render() {
    drawBackground()
    drawMiniMap()
    var cArc = parc

    cArc = cArc - Angle30
    if(cArc < 0)
      cArc = Angle360 + cArc

    var verticalGrid = 0
    var horizontalGrid = 0
    var dist2NextVerticalGrid = 0
    var dist2NextHorizontalGrid = 0
    var xIntersection = 0F
    var yIntersection = 0F
    var xGridIndex = 0
    var yGridIndex = 0
    var dist2VerticalGridBeingHit = 0F
    var dist2HorizontalGridBeingHit = 0F
    var dist2NextXIntersection = 0F
    var dist2NextYIntersection = 0F

    Range(0, ProjectionPlaneWidth , 5) map { castColumn =>
      if(cArc > Angle0 && cArc < Angle180) {
        horizontalGrid = (py / TileSize) * TileSize + TileSize // truncate then add
        dist2NextHorizontalGrid = TileSize
        val xTemp = TrigonometricTables.ITan(cArc) * (horizontalGrid - py)
        xIntersection = xTemp + px
      } else { // the ray is facing up
        horizontalGrid = (py / TileSize.value) * TileSize
        dist2NextHorizontalGrid = -1 * TileSize
        val xTemp = (TrigonometricTables.ITan(cArc) * (horizontalGrid - py))
        xIntersection = xTemp + px
        horizontalGrid = horizontalGrid - 1
      }

      if(cArc == Angle0.value || cArc == Angle180.value)
        dist2HorizontalGridBeingHit = 9999999F
      else {
        dist2NextXIntersection = TrigonometricTables.xStep(cArc)
        var FindRayBlocked = true
        while(FindRayBlocked) {
          xGridIndex = (xIntersection / TileSize)
          yGridIndex = (horizontalGrid / TileSize)
          if(xGridIndex >= MapWidth || yGridIndex >= MapHeight || xGridIndex < 0 || yGridIndex < 0) {
            dist2HorizontalGridBeingHit = Float.MaxValue
            FindRayBlocked = false
          } else if (map((yGridIndex * MapWidth) + xGridIndex) != Empty.value) {
            dist2HorizontalGridBeingHit = (xIntersection - px) * TrigonometricTables.ICos(cArc)
            FindRayBlocked = false
          } else {
            xIntersection = xIntersection + dist2NextXIntersection
            horizontalGrid = horizontalGrid + dist2NextHorizontalGrid
          }
        }
      }
      // X Ray
      if(cArc < Angle90 || cArc > Angle270) {
        verticalGrid = TileSize + (px / TileSize) * TileSize
        dist2NextVerticalGrid = TileSize
        val yTemp = TrigonometricTables.Tan(cArc) * (verticalGrid - px)
        yIntersection = yTemp + py
      } else { // ray facing left
        verticalGrid = (px / TileSize ) * TileSize
        dist2NextVerticalGrid = -1 * TileSize
        val yTemp = TrigonometricTables.Tan(cArc) * (verticalGrid - px)
        yIntersection = yTemp + py
        verticalGrid = verticalGrid - 1
      } // look for vertical wall

      if(cArc == Angle90.value || cArc == Angle270.value)
        dist2VerticalGridBeingHit = 9999999F
      else {
        dist2NextYIntersection = TrigonometricTables.yStep(cArc)
        var FindRayBlocked = true
        while(FindRayBlocked) {
          xGridIndex = (verticalGrid / TileSize)
          yGridIndex = (yIntersection / TileSize)
          if(xGridIndex >= MapWidth || yGridIndex >= MapHeight || xGridIndex < 0 || yGridIndex < 0) {
            dist2VerticalGridBeingHit = Float.MaxValue
            FindRayBlocked = false
          } else if (map((yGridIndex * MapWidth) + xGridIndex) != Empty.value) {
            dist2VerticalGridBeingHit = (yIntersection - py) * TrigonometricTables.ISin(cArc)
            FindRayBlocked = false
          } else {
            yIntersection = yIntersection + dist2NextYIntersection
            verticalGrid = verticalGrid + dist2NextVerticalGrid
          }
        }
      }

      //draw wall slice
      var dist = 0F
      var topOfWall = 0
      var bottonOfWall = 0
      if(dist2HorizontalGridBeingHit < dist2VerticalGridBeingHit) {
        drawDirectionOnMiniMap(xIntersection, horizontalGrid)
        dist = dist2HorizontalGridBeingHit
        offscreenGraphics.setColor(Color.gray)
      } else {
       drawDirectionOnMiniMap(verticalGrid, yIntersection)
        dist = dist2VerticalGridBeingHit
        offscreenGraphics.setColor(Color.darkGray)
      }
      dist = dist / TrigonometricTables.Fish(castColumn)
      val projectedWallHeight = WallHeight * playerDistToProjectionPlane / dist
      bottonOfWall = projectionPlaneYCenter + (projectedWallHeight * 0.5F)
      topOfWall = ProjectionPlaneHeight - bottonOfWall
      if(bottonOfWall >= ProjectionPlaneHeight)
        bottonOfWall = ProjectionPlaneHeight - 1
      offscreenGraphics.fillRect(castColumn, topOfWall, 5, projectedWallHeight)
      cArc = cArc + 5
      if(cArc >= Angle360)
        cArc = cArc - Angle360
    }
    paint(getGraphics)
  }

  override def paint(g : Graphics) {
    if(offscreenImg != null)
      g.drawImage(offscreenImg, 0, 0, this)
  }

  override def stop() {
    if(renderT != null && renderT.isAlive) {
      renderT.stop()
      renderT = null
    }
  }

  def keyEvent(set : Boolean)(key : Int) = {
    key match {
      case Event.UP => vkeyUp = set
      case 'w' => vkeyUp = set
      case 'W' => vkeyUp = set
      case Event.DOWN => vkeyDown = set
      case 's' => vkeyDown = set
      case 'S' => vkeyDown = set
      case Event.LEFT => vkeyLeft= set
      case 'a' => vkeyLeft= set
      case 'A' => vkeyLeft= set
      case Event.RIGHT => vkeyRight = set
      case 'd' => vkeyRight= set
      case 'D' => vkeyRight = set
      case _ => 1 == 1
    }
    true
  }

  override def keyDown(event : Event, key : Int) : Boolean = {
    keyEvent(true)(key)
  }

  override def keyUp(event : Event, key : Int) : Boolean = {
    keyEvent(false)(key)
  }

  implicit def float2int(f : Float) : Int = f.toInt
}
