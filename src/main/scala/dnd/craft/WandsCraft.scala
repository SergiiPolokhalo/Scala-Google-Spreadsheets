package dnd.craft

/**
 * Two types of wand
 */
object WandsCraft extends CasterMath {

  import Math.max

  def ethernalWand(mode: Mode = MODE_BASIC_CRAFT,
                   spellLevel: Int = 1,
                   casterLevel: Int = 1,
                   spellGpCost: Int = 0,
                   spellXpCost: Int = 0,
                   charges: Int = 2): List[Requirements] = {
    assert(spellLevel<=3,"Spell level maximum is 3")
    assert(charges==2,"Charges is 2 only")
    val totalPrice = spellLevel match {
      case 0 => 460
      case 1 => 820
      case 2 => 4420
      case 3 => 10900
      case _ => 0
    }
    List(
      craftResources(totalPrice, mode),
      UmdRequirements("Umd CL", round(craftDC(spellLevel)))
    )
  }

  //need a tests
  def regularWand(mode: Mode = MODE_BASIC_CRAFT,
                  spellLevel: Int = 1,
                  casterLevel: Int = 1,
                  spellGpCost: Int = 0,
                  spellXpCost: Int = 0,
                  charges: Int = 50
                 ): List[Requirements] = {
    assert(spellLevel<=4,"Spell level maximum is 3")
    assert(charges<=50,"Charges is 50 maximum")
    assert(minCL(0) == 1,"CL 0 == 1")
    assert(minCL(1) == 1,"CL 1 == 1")
    assert(minCL(2) == 3,"CL 2 == 3")
    assert(minCL(3) == 5,"CL 3 == 5")
    assert(minCL(4) == 7,"CL 4 == 7")

    val totalPrice = 15 * (
      effectiveSL(spellLevel)
        * max(casterLevel, minCL(spellLevel))
        + 5 * spellXpCost
        + spellGpCost
      ) * charges;
    List(
      craftResources(totalPrice, mode),
      UmdRequirements("Umd CL", round(craftDC(spellLevel)))
    )
  }
}
