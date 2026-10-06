package pcd.ass03.model

final case class V2d(x: Double, y: Double):

  def sum(v: V2d): V2d =
    V2d(
      x + v.x,
      y + v.y
    )

  def sub(v: V2d): V2d =
    V2d(
      x - v.x,
      y - v.y
    )

  def mul(factor: Double): V2d =
    V2d(
      x * factor,
      y * factor
    )

  def abs: Double =
    math.sqrt(
      x * x + y * y
    )

  def getNormalized: V2d =

    val module = abs

    if module == 0 then
      V2d(0, 0)
    else
      V2d(
        x / module,
        y / module
      )

  override def toString: String =
    s"V2d($x,$y)"