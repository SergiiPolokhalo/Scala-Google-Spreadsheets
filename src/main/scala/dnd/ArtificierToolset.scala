package dnd

import cells.Cell
import cells.Cell.{Data, VectorToJS}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.{Failure, Success, Try}

object ArtificierToolset {
  /**
   * Create wondrous item by base price
   */
  @JSExportTopLevel("WONDROUS")
  def wondrous(total:Int): js.Array[js.Array[Data]] = {
    val cl = 0
    val artTotal = total * 0.75
    val locTotal = artTotal * 0.7
    report (
      Vector(
        ReportRow(total, 0, 0, 0),
        ReportRow(total / 2, atLeastOne(total, 25), atLeastOne(total, 1000), cl, "simple"),
        ReportRow(artTotal / 2, atLeastOne(artTotal, 25), atLeastOne(artTotal, 1000), cl, "feats"),
        ReportRow(locTotal / 2, atLeastOne(locTotal, 25), atLeastOne(locTotal, 1000), cl, "restricted")
      )
    )
  }

  /**
   * Create Artificer Scroll
   *
   * @param spellLevel  {spellLevel} spell level
   * @param casterLevel {casterLevel} caster level
   * @param spellCost   {spellCost} spell cost
   * @param spellXpCost {spellXpCost} spell xp cost
   * @return cost creation of Artificer scroll
   */
  @JSExportTopLevel("SCROLL")
  def scroll(spellLevel: Int, casterLevel: Int, spellCost: Int, spellXpCost: Int): js.Array[js.Array[Data]] = scroll(spellLevel, casterLevel, spellCost, spellXpCost, 1)

  /**
   * Create Artificer Scroll
   *
   * @param spellLevel  {spellLevel} spell level
   * @param casterLevel {casterLevel} caster level
   * @param spellCost   {spellCost} spell cost
   * @param spellXpCost {spellXpCost} spell xp cost
   * @param charges     {charges} spell on scroll
   * @return cost creation of Artificer scroll
   */
  @JSExportTopLevel("SCROLL")
  def scroll(
              spellLevel: Int,
              casterLevel: Int,
              spellCost: Int,
              spellXpCost: Int,
              charges: Int
            ): js.Array[js.Array[Data]] = {
    report(
      count(
        25,
        spellLevel,
        casterLevel,
        spellCost,
        spellXpCost,
        charges
      )
    )
  }

  def count(costFactor: Int, spellLevel: Int, casterLevel: Int, spellCost: Int, spellXpCost: Int, charges: Int): Vector[ReportRow] = {
    val sl = zeroSpellFix(spellLevel)
    val cl = Math.max(casterLevel, minimalCasterLevel(sl)).toInt
    val oneShoot = costFactor * sl * cl + spellCost + 5 * spellXpCost
    val total = charges * oneShoot
    val artTotal = total * 0.75
    val locTotal = artTotal * 0.7
    Vector(
      ReportRow(total, 0, 0, 0),
      ReportRow(total / 2, atLeastOne(total, 25), atLeastOne(total, 1000), cl, "simple"),
      ReportRow(artTotal / 2, atLeastOne(artTotal, 25), atLeastOne(artTotal, 1000), cl, "feats"),
      ReportRow(locTotal / 2, atLeastOne(locTotal, 25), atLeastOne(locTotal, 1000), cl, "restricted")
    )
  }

  def atLeastOne(v: Double, w: Double): Int = Try {
    v / w
  } match {
    case Failure(_) => 1
    case Success(value) => value match {
      case n if n <= 0 => 1
      case n =>Math.round(n+0.5).toInt
    }
  }

  def zeroSpellFix(spellLevel: Int): Double = Math.max(spellLevel, 0.5)

  def minimalCasterLevel(spellLevel: Double): Double = Math.max(1, spellLevel * 2 - 1)

  def report(v: Vector[ReportRow]): js.Array[js.Array[Data]] = {
    (Vector(Vector(Cell("GOLD"), Cell("XP"), Cell("DAYS"), Cell("CL"), Cell("MODE"))) ++ v.map(_.toRow)).toGoogleCells
  }

  /**
   * Create Artificer Scroll
   *
   * @param spellLevel  {spellLevel} spell level
   * @param casterLevel {casterLevel} caster level
   * @param spellCost   {spellCost} spell cost
   * @return cost creation of Artificer scroll
   */
  @JSExportTopLevel("SCROLL")
  def scroll(spellLevel: Int, casterLevel: Int, spellCost: Int): js.Array[js.Array[Data]] = scroll(spellLevel, casterLevel, spellCost, 0, 1)

  /**
   * Create Artificer Scroll
   *
   * @param spellLevel  {spellLevel} spell level
   * @param casterLevel {casterLevel} caster level
   * @return cost creation of Artificer scroll
   */
  @JSExportTopLevel("SCROLL")
  def scroll(spellLevel: Int, casterLevel: Int): js.Array[js.Array[Data]] = scroll(spellLevel, casterLevel, 0, 0, 1)

  /**
   * Create Artificer Scroll
   *
   * @param spellLevel {spellLevel} spell level
   * @return cost creation of Artificer scroll
   */
  @JSExportTopLevel("SCROLL")
  def scroll(spellLevel: Int): js.Array[js.Array[Data]] = scroll(spellLevel, 0, 0, 0, 1)

  /**
   * Create Artificer wand
   *
   * @param spellLevel  {spellLevel} spell level
   * @param casterLevel {casterLevel} caster level
   * @param spellCost   {spellCost} spell cost
   * @param spellXpCost {spellXpCost} spell xp cost
   * @return cost creation of Artificer scroll
   */
  @JSExportTopLevel("WAND")
  def wand(
            spellLevel: Int,
            casterLevel: Int,
            spellCost: Int,
            spellXpCost: Int
          ): js.Array[js.Array[Data]] = {
    wand(spellLevel, casterLevel, spellCost, spellXpCost, 50)
  }

  /**
   * Create Artificer wand
   *
   * @param spellLevel  {spellLevel} spell level
   * @param casterLevel {casterLevel} caster level
   * @param spellCost   {spellCost} spell cost
   * @return cost creation of Artificer scroll
   */
  @JSExportTopLevel("WAND")
  def wand(
            spellLevel: Int,
            casterLevel: Int,
            spellCost: Int
          ): js.Array[js.Array[Data]] = {
    wand(spellLevel, casterLevel, spellCost, 0, 50)
  }

  /**
   * Create Artificer wand
   *
   * @param spellLevel {spellLevel} spell level
   * @return cost creation of Artificer scroll
   */
  @JSExportTopLevel("WAND")
  def wand(
            spellLevel: Int
          ): js.Array[js.Array[Data]] = {
    wand(spellLevel, 0, 0, 0, 50)
  }

  /**
   * Create Artificer wand
   *
   * @param spellLevel  {spellLevel} spell level
   * @param casterLevel {casterLevel} caster level
   * @param spellCost   {spellCost} spell cost
   * @param spellXpCost {spellXpCost} spell xp cost
   * @param charges     {charges} charges on scroll
   * @return cost creation of Artificer wand
   */
  @JSExportTopLevel("WAND")
  def wand(
            spellLevel: Int,
            casterLevel: Int,
            spellCost: Int,
            spellXpCost: Int,
            charges: Int
          ): js.Array[js.Array[Data]] = {
    report(
      count(
        15,
        spellLevel,
        casterLevel,
        spellCost,
        spellXpCost,
        charges
      )
    )
  }

  case class ReportRow(price: Double, xp: Int, days: Int, casterLevel: Int, name: String = "") {
    def toRow = Vector(
      Cell(price),
      Cell(xp),
      Cell(days),
      Cell(casterLevel),
      Cell(name)
    )
  }

}
