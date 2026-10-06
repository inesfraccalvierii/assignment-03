package pcd.ass03.model

final case class P2d(x: Double, y: Double):

  def sum(v: V2d): P2d =
    P2d(
      x + v.x,
      y + v.y
    )

  def sub(v: P2d): V2d =
    V2d(
      x - v.x,
      y - v.y
    )

  def distance(v: P2d): Double =
    val dx = v.x - x
    val dy = v.y - y

    math.sqrt(
      dx * dx + dy * dy
    )

  def wrapped(
               minX: Double,
               maxX: Double,
               minY: Double,
               maxY: Double
             ): P2d =

    val width = maxX - minX
    val height = maxY - minY

    val wrappedX =
      ((x - minX) % width + width) % width + minX

    val wrappedY =
      ((y - minY) % height + height) % height + minY

    P2d(
      wrappedX,
      wrappedY
    )

  override def toString: String =
    s"P2d($x,$y)"