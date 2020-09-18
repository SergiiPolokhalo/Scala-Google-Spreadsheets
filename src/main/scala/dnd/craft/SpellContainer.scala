package dnd.craft

import dnd.craft

trait CraftMath {
  val oneOrMore: Double => Int = {
    case 0 => 1
    case v@i if i.toInt == v => i.toInt
    case i => (1 + i).toInt
  }
  val rules: Double => List[String] = t => List(formatter(t / 2), formatterStrict(oneOrMore(t / 25)), formatterStrict(oneOrMore(t / 1000)))
  val modes: List[(String, Double)] = List(("Standard", 1.0), ("Feats", 0.75), ("Feats with restrictions", 0.75 * 0.7))

  def formatter(x: Double): String = f"$x%.2f"

  def formatterStrict(x: Double): String = f"$x%.0f"

  def buildByCost(totalCost: Int): List[List[String]] = modes.map {
    case (description, coefficient) => List(description) ::: rules(totalCost * coefficient)
  }
}

trait Craftable extends CraftMath {

  import Math.max

  lazy val cases: List[List[String]] = modes.map {
    case (description, coefficient) => List(description) ::: rules(totalPrice * coefficient) ::: formatterStrict(casterLevel) :: Nil
  }

  def sl: Int

  def cl: Int

  def cost: Int

  def xp: Int

  def shots: Int

  def shotCost: Int

  def totalPrice: Double = shotCost * (spellLevel * casterLevel + cost + 5 * xp) * shots

  def spellLevel: Double = if (sl <= 0) 0.5 else sl

  def casterLevel: Double = max(1, max(spellLevel * 2 - 1, cl))
}

/**
 * Spell scroll, usually have one spell, but some have more
 * in any case one spell per round
 *
 * @param sl    Spell level
 * @param cl    Caster level, can be recalculated inside if need
 * @param cost  Cost in gold (if applied for) of material component for one spell
 * @param xp    Cost in xp (if applied for) of component for one spell
 * @param shots count of spell in scroll, usually 1
 */
case class Scroll(
                   override val sl: Int,
                   override val cl: Int = 0,
                   override val cost: Int = 0,
                   override val xp: Int = 0,
                   override val shots: Int = 1
                 ) extends Craftable {

  override val shotCost: Int = 25

  override def toString: String = s"Scroll[SL:$spellLevel CL:$casterLevel Charges:$shots Price:$totalPrice]"

  println("Scroll scribed")
}

/**
 * Magic wand, usually have charges, but some have less (used, as an example)
 * in any case one spell per round
 *
 * @param sl    Spell level (max level is 4)
 * @param cl    Caster level, can be recalculated inside if need
 * @param cost  Cost in gold (if applied for) of material component for one spell
 * @param xp    Cost in xp (if applied for) of component for one spell
 * @param shots count of spell in scroll, usually 50
 */
case class Wand(
                 override val sl: Int,
                 override val cl: Int = 0,
                 override val cost: Int = 0,
                 override val xp: Int = 0,
                 override val shots: Int = 50
               ) extends Craftable {

  assume(sl <= 4, "Sorry but Wand can be contain only up to 4th spell level")

  override val shotCost: Int = 15

  override def toString: String = s"Wand[SL:$spellLevel CL:$casterLevel Charges:$shots Price:$totalPrice]"

  println("Wand created")
}

case class Wondrous(cost:Int, name:String) extends CraftMath {
  override def toString: String = s"Wondrous Item[$name Price:$cost]"
}

/**
 * JS proxy
 */
object ArtificerBuilder {
  private val holder = scala.collection.mutable.Map[(Int,Int,Int,Int,Int,String),CraftMath]()

  def getOrBuildScroll(sl: Int, cl: Int, cost: Int, xp: Int, shots: Int): craft.Scroll = {
    holder.getOrElseUpdate((sl,cl,cost,xp,shots,"S"), Scroll(sl,cl,cost,xp,shots)).asInstanceOf[craft.Scroll]
  }

  def scroll(sl: Int,
             cl: Int,
             cost: Int,
             xp: Int,
             shots: Int
            ):craft.Scroll =   getOrBuildScroll(sl,cl,cost,xp,shots)

  def getOrBuildWand(sl: Int, cl: Int, cost: Int, xp: Int, shots: Int): craft.Wand = {
    holder.getOrElseUpdate((sl,cl,cost,xp,shots,"W"), Wand(sl,cl,cost,xp,shots)).asInstanceOf[craft.Wand]
  }

  def wand(sl: Int,
           cl: Int,
           cost: Int,
           xp: Int,
           shots: Int
          ):craft.Wand =  getOrBuildWand(sl,cl,cost,xp,shots)

  def main(args: Array[String]): Unit = {
    val w1 = wand(1,1,0,0,50)
    val w2 = wand(1,1,0,0,50)
    val s1 = scroll(1,1,0,0,5)
    val s2 = scroll(1,1,0,0,6)

  }
}