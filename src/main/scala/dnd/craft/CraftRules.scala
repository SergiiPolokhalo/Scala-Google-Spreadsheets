package dnd.craft

trait CasterMath {

  import Math.max

  type Mode = (Double, String)
  val MODE_BASIC_CRAFT: Mode = (1.0, "Basic")
  val MODE_ARTISAN_CRAFT: Mode = (0.75, "Artisan")
  val MODE_RESTRICTED_CRAFT: Mode = (0.7 * 0.75, "Restricted Artisan")

  def minCL(sL: Int): Double = max(1, effectiveSL(sL) * 2 - 1)

  def craftDC(sL: Int): Double = 20 + effectiveSL(sL)

  def effectiveSL(sL: Int): Double = max(sL, 0.5)

  def craftResources(totalPrice: Double, mode: Mode): Requirements = CraftRequirements(
    round2(mode._1 * totalPrice / 2),
    round(mode._1 * totalPrice / 25),
    round(mode._1 * totalPrice / 1000),
    mode
  )

  def round(v: Double): Int = if (v == v.toInt) v.toInt else v.toInt + 1

  def round2(v: Double): Double = (v * 100 + 0.5).toInt / 100.00

  trait Requirements

  case class CraftRequirements(gold: Double, xp: Int, days: Int, mode: Mode) extends Requirements

  case class UmdRequirements(name: String, dc: Int) extends Requirements

  case class OtherRequirements(name: String) extends Requirements
}

object CraftRules extends CasterMath with App {
  println(minCL(0), minCL(1), minCL(4))
  println(craftResources(16000, MODE_RESTRICTED_CRAFT))
  List(
    WandsCraft.ethernalWand(MODE_BASIC_CRAFT,3),
    WandsCraft.ethernalWand(MODE_ARTISAN_CRAFT,3),
    WandsCraft.ethernalWand(MODE_RESTRICTED_CRAFT,3),
    WandsCraft.regularWand(MODE_BASIC_CRAFT,4),
    WandsCraft.regularWand(MODE_ARTISAN_CRAFT,4),
    WandsCraft.regularWand(MODE_RESTRICTED_CRAFT,4),
  ).foreach(println)
}
