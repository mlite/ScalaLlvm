package org.scalair.hoopl

import TypeDefines._
import collection.immutable.HashMap
import org.scalair.util.Logging

/**
 * User: wangn
 * Date: 6/17/11
 */

object Block{

}

trait ShapeLifter[N<:NonLocal,E<:Status,X<:Status] extends EXShape[E,X] with HoopMonad {
  def singletonDG[F<:Ordered[F]]: F => DG[F,N,E,X] = throw new Exception()
  def fwdEntryFact[F<:Ordered[F]]: F => Fact[E,F] = throw new Exception()
  def fwdEntryLabel:MaybeC[E, List[BlockId]] = throw new Exception()
  def ftransfer[F<:Ordered[F]]:FwdPass[N,F]=>F=> Fact[X,F] = throw new Exception()
  def frewrite[F<:Ordered[F]]:FwdPass[N,F]=>F=>CMM#M[Option[(Graph[N,E,X],HooplFwdRewrite[N,F])]]
  = throw new Exception()

  def bwdEntryFact[F<:Ordered[F]]: DataflowLattice[F] => Fact[E,F] => F = throw new Exception()
  def btransfer[F<:Ordered[F]]: BwdPass[N,F]=> Fact[X,F]=>F = throw new Exception()
  def brewrite[F<:Ordered[F]]: BwdPass[N,F]=> Fact[X,F]=> CMM#M[Option[(Graph[N,E,X],HooplBwdRewrite[N,F])]]
  = throw new Exception()
}

sealed abstract class Block[N<:NonLocal,E<:Status,X<:Status]()
  extends ShapeLifter[N,E,X] with NonLocal with LabelsPtr with Dot with Logging {
  type HMImpl = Block[N,E,X]
  type EXShapleImpl[e<:Status,x<:Status] = Block[N,e,x]

  def frontBiasBlock:Block[N,E,X]
  def backBiasBlock:Block[N,E,X]

  def append(x1:Block[N,O,O], x2:Block[N,O,O]):Block[N,O,O] = x1 match {
    case BCat(b1,b2) => append(b1, append(b2, x2))
    case BMiddle(_) => BCat[N](x1, x2)
    case _ => throw new Exception()
  }

  def targetBlockIds():List[BlockId] = succBlockIds

  def map3[N1<:NonLocal](first:N=>N1, middle:N=>N1, last:N=>N1):Block[N1,E,X]
  def map[N1<:NonLocal](f:N=>N1):Block[N1,E,X] = map3[N1](f,f,f)

  def cat[X1<:Status](that:Block[N,O,X1]):Block[N,E,X1]
  def addToRight(that:Block[N,O,C]):Block[N,O,C] = throw new Exception()
  def addToLeft(g:Block[N,C,O]):Block[N,C,O] = throw new Exception()

  def toDot = makeNode("block" + blockId, toDotLabelTable)
  def toInstStr:String


  def node:N

  def arfNode[F<:Ordered[F]]:FwdPass[N,F]=>F=>CMM#M[(Graph[N,E,X], Fact[X,F])] = {
    pass => f => frewrite(pass)(f) flatMap { grw =>
      grw match {
        case None => cmObj._return[(Graph[N,E,X], Fact[X,F])]((singletonDG[F](f), ftransfer(pass)(f)))
        case Some((g,rw)) => {
          val pass1 = new FwdPass[N,F] {
            val lattice = pass.lattice
            val transfer = pass.transfer
            val rewrite = rw
          }
          val f1 = fwdEntryFact[F](f)
          g.arf[F](pass1)(fwdEntryLabel)(f1)
        }
      }
    }
  }

  def arbNode[F<:Ordered[F]]:BwdPass[N,F]=>Fact[X,F]=>CMM#M[(Graph[N,E,X], Fact[X,F])] = {
    pass => fact => brewrite(pass)(fact) flatMap { bwdres =>
      bwdres match {
        case None => {
          val entry_f = btransfer(pass)(fact)
          cmObj._return[(Graph[N,E,X], Fact[X,F])]((singletonDG[F](entry_f), Left(entry_f)))
        }
        case Some((g,rw)) => {
          val pass1 = new BwdPass[N,F] {
            val lattice = pass.lattice
            val transfer = pass.transfer
            val rewrite = rw
          }
          g.arb[F](pass1)(fwdEntryLabel)(fact) flatMap { case (g, f) =>
            cmObj._return((g, Left(bwdEntryFact(pass.lattice)(f))))
          }
        }
      }
    }
  }

  def arf[F<:Ordered[F]]:FwdPass[N,F]=>Fact[E,F]=>CMM#M[(Graph[N,E,X], Fact[X,F])] = { pass => f=>
    //arfNode(node)(pass)(f.left.get)
    arfNode(pass)(f.left.get)
  }

  def arb[F<:Ordered[F]]:BwdPass[N,F]=>Fact[X,F]=>CMM#M[(Graph[N,E,X], Fact[X,F])] = { pass => f=>
    arbNode(pass)(f)
  }


  def arfx[F<:Ordered[F]]:FwdPass[N,F]=>Fact[C,F]=>CMM#M[(Graph[N,E,X], Fact[X,F])] = { pass=>fact=>
    val fOpt = pass.lattice.joinInFacts(fact.right.get).get(blockId)
    arf[F](pass)(Left(fOpt.get))
  }

  def arbx[F<:Ordered[F]]:BwdPass[N,F]=>Fact[C,F]=>CMM#M[(Graph[N,E,X], Fact[X,F])] = { pass=>fact=>
    arb[F](pass)(fact) flatMap { case (rg,f) =>
      val fb = pass.lattice.joinInFacts(mapSingleton(blockId, f.left.get))
      cmObj._return((rg,Right(fb)))
    }
  }

  def do_block[F<:Ordered[F]]:Pass[F] => FactBase[F] => CMM#M[(Graph[N,E,X], Fact[X,F])] = {
    pass => fbase => pass match {
      case fwd:FwdPass[N,F] => {
        val entryFact = pass.lattice.getFact(blockId, fbase)
        arf[F](fwd)(Left(entryFact))
      }
      case bwd:BwdPass[N,F] => {
        arb[F](bwd)(Right(fbase)) flatMap { case (g,f) =>
          cmObj._return((g, Right(mapSingleton(blockId, f.left.get))))
        }
      }
    }
  }

  def tx_block[F<:Ordered[F]]:Pass[F]=>List[BlockId]=>TxFactBase[N,F]=>cmObj.M[TxFactBase[N,F]] = {
    pass=>in_lbls=>tx_fb=> {
      assert(e == C() && x == C(), "only applicable to Block[N,C,C]")
      val lbls = tx_fb.lbls
      val lbls1 = lbls ++ in_lbls.toSet
      val fbase = tx_fb.fbase
      val blks = tx_fb.rg
      pass match {
        case fwd:FwdPass[N,F] if (!fbase.contains(blockId)) =>
        // Unreachable blocks
          cmObj._return[TxFactBase[N,F]](TxFactBase[N,F](fbase, tx_fb.rg, tx_fb.cha, lbls1))
        case _ => {
          do_block(pass)(fbase) flatMap { case (rg, out_facts) =>
            logger.debug("[tx_block] update facts to lbls: {}", lbls)
            logger.debug("[tx_block] update facts to rg: {}", rg)
            logger.debug("[tx_block.fbase] {}", fbase)
            logger.debug("[tx_block.out_facts] {}", out_facts)
            val (cha1, fbase1) = out_facts.right.get.foldLeft((tx_fb.cha,fbase)) { (p, e) =>
              pass.lattice.updateFact(lbls, e._1, e._2, p)
            }
            logger.debug("[tx_block.fbase1] {}", fbase1)
            logger.debug("[tx_block.cha1] {}", cha1)
            val rg1 = rg.splice[C](blks.castEX[X,C]).castEX[C,C]
            cmObj._return[TxFactBase[N,F]](TxFactBase[N,F](fbase1, rg1, cha1, lbls1))
          }
        }
      }
    }
  }

  def dropFact:Block[N,E,X] = throw new Exception()
  def distributeFact[F<:Ordered[F]]: F => FactBase[F] = throw new Exception()
}

abstract class COBlock[N<:NonLocal]() extends Block[N,C,O]() with COShape {

  override def singletonDG[F<:Ordered[F]] = { fact:F => Graph.gUnitCO[N](DBlock[N,C,O,F](this, fact)) }
  override def fwdEntryLabel = JustC(List(blockId))
  override def fwdEntryFact[F<:Ordered[F]] = { fact:F => Right(mapSingleton[F](blockId, fact)) }
  override def bwdEntryFact[F<:Ordered[F]] = { lat:DataflowLattice[F] => fact:Fact[C,F] => lat.getFact(blockId, fact.right.get) }
  override def ftransfer[F<:Ordered[F]] = { fwdPass:FwdPass[N,F] =>f:F => Left(fwdPass.transfer.first(this)(f)) }
  override def btransfer[F<:Ordered[F]] = { bwdPass:BwdPass[N,F] =>fact:Fact[O,F] => bwdPass.transfer.first(this)(fact.left.get) }
  override def frewrite[F<:Ordered[F]] = { fwdPass:FwdPass[N,F] => fwdPass.rewrite.first(this) }
  override def brewrite[F<:Ordered[F]] = { bwdPass:BwdPass[N,F] => fact:Fact[O,F] => bwdPass.rewrite.first(this)(fact.left.get) }
  def distributeFactBwd[F<:Ordered[F]]:F => FactBase[F] = { f => mapSingleton(blockId, f) }
}

abstract class OOBlock[N<:NonLocal]() extends Block[N,O,O]() with OOShape {

  override def singletonDG[F<:Ordered[F]] = { fact:F => Graph.gUnitOO[N](DBlock[N,O,O,F](this, fact)) }
  override def fwdEntryLabel = NothingC[List[BlockId]]
  override def fwdEntryFact[F<:Ordered[F]] = { fact:F => Left(fact) }
  override def bwdEntryFact[F<:Ordered[F]] = { lat:DataflowLattice[F] => fact:Fact[C,F] => fact.left.get }
  override def ftransfer[F<:Ordered[F]] = { fwdPass:FwdPass[N,F] =>f:F=> Left(fwdPass.transfer.middle(this)(f)) }
  override def btransfer[F<:Ordered[F]] = { bwdPass:BwdPass[N,F] =>fact:Fact[O,F] => bwdPass.transfer.middle(this)(fact.left.get) }
  override def frewrite[F<:Ordered[F]] = { fwdPass:FwdPass[N,F] => fwdPass.rewrite.middle(this) }
  override def brewrite[F<:Ordered[F]] = { bwdPass:BwdPass[N,F] =>fact:Fact[O,F] => bwdPass.rewrite.middle(this)(fact.left.get) }
}

abstract class OCBlock[N<:NonLocal]() extends Block[N,O,C]() with OCShape  {

  override def singletonDG[F<:Ordered[F]] = { fact:F => Graph.gUnitOC[N](DBlock[N,O,C,F](this, fact)) }
  override def fwdEntryLabel = NothingC[List[BlockId]]
  override def fwdEntryFact[F<:Ordered[F]] = { fact:F => Left(fact) }
  override def bwdEntryFact[F<:Ordered[F]] = { lat:DataflowLattice[F] => fact:Fact[C,F] => fact.left.get }
  override def ftransfer[F<:Ordered[F]] = { fwdPass:FwdPass[N,F]=>f:F=> Right(fwdPass.transfer.last(this)(f)) }
  override def btransfer[F<:Ordered[F]] = { bwdPass:BwdPass[N,F]=>fact:Fact[C,F] => bwdPass.transfer.last(this)(fact.right.get) }
  override def frewrite[F<:Ordered[F]] = { fwdPass:FwdPass[N,F] => fwdPass.rewrite.last(this) }
  override def brewrite[F<:Ordered[F]] = { bwdPass:BwdPass[N,F] => fact:Fact[C,F] => bwdPass.rewrite.last(this)(fact.right.get) }
  override def distributeFact[F<:Ordered[F]]: F => FactBase[F] = { f =>
    succBlockIds.foldLeft(HashMap[BlockId, F]()) { (p, e) =>
      logger.debug("[distributeFact to successors ] {} ===>", f, e)
      p + (e -> f)
    }
  }

  def successorFacts[F<:Ordered[F]]:FactBase[F]=>List[F]={ fbase =>
    succBlockIds.map {id =>
      fbase.get(id) match {
        case Some(f) => f
        case None => throw new Exception()
      }
    }
  }
}


final case class BFirst[N<:NonLocal](val node:N) extends COBlock[N]() {

  def blockId():BlockId = node.blockId()
  def succBlockIds():List[BlockId] = List()
  def frontBiasBlock = this
  def backBiasBlock = this
  def map3[N1<:NonLocal](f:N=>N1, m:N=>N1, l:N=>N1) = BFirst[N1](f(node))

  def cat[X1<:Status](that:Block[N,O,X1]) = that match {
    case (x2:BMiddle[N]) => BHead[N](this,x2)
    case (x2:BCat[N]) => cat[O](x2.m1).cat[O](x2.m2)
    case (x2:BLast[N]) => BClosed[N](this,x2)
    case (x2:BTail[N]) => BClosed[N](this,x2)
    case _ => throw new Exception()
  }

  def toDotLabelRow = makeLeftRow(colorFont(node.toString, "white"), "black")
  override def toDotLabelTable = throw new Exception()
  def toInstStr = node.toString
}


final case class BMiddle[N<:NonLocal](val node:N) extends OOBlock[N]() {

  def blockId():BlockId = node.blockId()
  def succBlockIds():List[BlockId] = throw new Exception()
  def frontBiasBlock = this
  def backBiasBlock = this
  def map3[N1<:NonLocal](f:N=>N1, m:N=>N1, l:N=>N1) = BMiddle[N1](m(node))

  def cat[X1<:Status](that:Block[N,O,X1]) = that match {
    case x2:BMiddle[N] => BCat[N](this, x2)
    case x2:BCat[N] => BCat[N](this, x2)
    case x2:BLast[N] => BTail[N](this, x2)
    case x2:BTail[N] => BTail[N](this, x2)
    case _ => throw new Exception()
  }

  override def addToRight(that:Block[N,O,C]) = BTail[N](this, that)
  override def addToLeft(that:Block[N,C,O]) = BHead[N](that, this)
  def toDotLabelRow = makeLeftRow(node.toString, "white")
  override def toDotLabelTable = throw new Exception()
  def toInstStr = node.toString
}

final case class BLast[N<:NonLocal](val node:N) extends OCBlock[N]() {

  def blockId():BlockId = throw new Exception()
  def succBlockIds():List[BlockId] = node.succBlockIds()
  def frontBiasBlock = this
  def backBiasBlock = this
  def map3[N1<:NonLocal](f:N=>N1, m:N=>N1, l:N=>N1) = BLast[N1](l(node))
  def cat[X1<:Status](that:Block[N,O,X1]) = throw new Exception("cannot append a node to last")

  def toDotLabelRow = makeLeftRow(node.toString, "white")
  def toInstStr = node.toString
}

final case class BCat[N<:NonLocal](val m1:Block[N,O,O], val m2:Block[N,O,O]) extends OOBlock[N]() {

  def node:N = throw new Exception()
  def blockId():BlockId = m1.blockId()
  def succBlockIds():List[BlockId] = m2.succBlockIds()
  def frontBiasBlock = {
    def rotateRight(x:Block[N,O,O]):Block[N,O,O] = x match {
      case BCat(h,t) => append(h, rotateRight(t))
      case BMiddle(_) => x
      case _ => throw new Exception()
    }
    rotateRight(this)
  }

  def backBiasBlock = {
    def rotateLeft(x:Block[N,O,O]):Block[N,O,O] = x match {
      case BCat(h,t) => append(rotateLeft(h), t)
      case BMiddle(_) => x
      case _ => throw new Exception()
    }
    rotateLeft(this)
  }

  def map3[N1<:NonLocal](f:N=>N1, m:N=>N1, l:N=>N1) =
    BCat[N1](m1.map3(f,m,l), m2.map3(f,m,l))

  def cat[X1<:Status](that:Block[N,O,X1]) = that match {
    case x3:BLast[N] => m1.cat[C](m2.cat[C](x3))
    case x3:BTail[N] => m1.cat[C](m2.cat[C](x3))
    case x2:BCat[N] => BCat[N](this, x2)
    case x2:BMiddle[N]  => BCat[N](this, x2)
    case _ => throw new Exception()
  }

  override def addToRight(that:Block[N,O,C]) = m1.addToRight(m2.addToRight(that))

  override def addToLeft(that:Block[N,C,O]) = m2.addToLeft(m1.addToLeft(that))

  def toDotLabelRow = m1.toDotLabelRow + m2.toDotLabelRow
  def toInstStr = m1.toInstStr + "\n" + m2.toInstStr

  override def arf[F<:Ordered[F]]:FwdPass[N,F] => Fact[O,F] => CMM#M[(Graph[N,O,O], Fact[O,F])] = { pass => f =>
    Graph.arfCat[N,O,O,O,F](m1.arf[F](pass), m2.arf[F](pass))(f)
  }
  override def arb[F<:Ordered[F]]:BwdPass[N,F] => Fact[O,F] => CMM#M[(Graph[N,O,O], Fact[O,F])] = { pass => f =>
    Graph.arbCat[N,O,O,O,F](m1.arb[F](pass), m2.arb[F](pass))(f)
  }
}

final case class BHead[N<:NonLocal](val first:Block[N,C,O], val middle:Block[N,O,O]) extends COBlock[N]() {

  def node:N = throw new Exception()
  def blockId():BlockId = first.blockId()
  def succBlockIds():List[BlockId] = middle.succBlockIds()
  def frontBiasBlock = this
  def backBiasBlock = this

  def map3[N1<:NonLocal](f:N=>N1, m:N=>N1, l:N=>N1) = BHead[N1](first.map3(f,m,l), middle.map3(f,m,l))

  def cat[X1<:Status](that:Block[N,O,X1]) = that match {
    case x2:BCat[N] => cat[O](x2.m1).cat[O](x2.m2)
    case x2:BMiddle[N] => BHead[N](this, x2)
    case x2:BLast[N] => BClosed[N](this, x2)
    case x2:BTail[N] => BClosed[N](this, x2)
    case _ => throw new Exception()
  }

  def toDotLabelRow = first.toDotLabelRow + middle.toDotLabelRow
  override def toDotLabelTable = throw new Exception()
  def toInstStr = first.toInstStr + "\n" + middle.toInstStr

  override def arf[F<:Ordered[F]]:FwdPass[N,F]=>Fact[C,F]=>CMM#M[(Graph[N,C,O], Fact[O,F])] = { pass => f =>
    Graph.arfCat[N,C,O,O,F](first.arf[F](pass), middle.arf[F](pass))(f)
  }

  override def arb[F<:Ordered[F]]:BwdPass[N,F]=>Fact[C,F]=>CMM#M[(Graph[N,C,O], Fact[O,F])] = { pass => f =>
    Graph.arbCat[N,C,O,O,F](first.arb[F](pass), middle.arb[F](pass))(f)
  }
}

final case class BTail[N<:NonLocal](val middle:Block[N,O,O], last:Block[N,O,C]) extends OCBlock[N]() {

  def node:N = throw new Exception()
  def blockId():BlockId = middle.blockId()
  def succBlockIds():List[BlockId] = last.succBlockIds()
  def frontBiasBlock = this
  def backBiasBlock = this

  def map3[N1<:NonLocal](f:N=>N1, m:N=>N1, l:N=>N1) = BTail[N1](middle.map3(f,m,l), last.map3(f,m,l))

  def cat[X1<:Status](that:Block[N,O,X1]) = throw new Exception("cannot add a node to BTail")
  def toDotLabelRow = middle.toDotLabelRow + last.toDotLabelRow

  def toInstStr = middle.toInstStr + "\n" + last.toInstStr

  override def arf[F<:Ordered[F]]:FwdPass[N,F] => Fact[O,F] => CMM#M[(Graph[N,O,C], Fact[C,F])] = { pass => f =>
    Graph.arfCat[N,O,O,C,F](middle.arf[F](pass), last.arf[F](pass))(f)
  }
  override def arb[F<:Ordered[F]]:BwdPass[N,F] => Fact[O,F] => CMM#M[(Graph[N,O,C], Fact[C,F])] = { pass => f =>
    Graph.arbCat[N,O,O,C,F](middle.arb[F](pass), last.arb[F](pass))(f)
  }
}

final case class BClosed[N<:NonLocal](first:Block[N,C,O], last:Block[N,O,C]) extends Block[N,C,C]() with CCShape {

  def node:N = throw new Exception()
  override def blockId():BlockId = first.blockId()
  override def succBlockIds():List[BlockId]= last.succBlockIds()

  def frontBiasBlock = {
    def shiftRight(h:Block[N,C,O], t:Block[N,O,C]):Block[N,C,C] = h match {
      case BHead(b1,b2) => shiftRight(b1, BTail(b2,t))
      case BFirst(_) => BClosed(h,t)
      case _ => throw new Exception()
    }
    shiftRight(first, last)
  }

  def backBiasBlock = {
    def shiftLeft(h:Block[N,C,O], t:Block[N,O,C]):Block[N,C,C] = t match {
      case BTail(t1,t2) => shiftLeft(BHead(h,t1),t2)
      case BLast(_) => BClosed(h,t)
      case _ => throw new Exception()
    }
    shiftLeft(first, last)
  }

  def map3[N1<:NonLocal](f:N=>N1, m:N=>N1, l:N=>N1) =
    BClosed[N1](first.map3(f,m,l), last.map3(f, m, l))

  def cat[X1<:Status](that:Block[N,O,X1]) = throw new Exception ("cannot add a node to closed node")

  def toDotLabelRow = first.toDotLabelRow + last.toDotLabelRow
  def toInstStr = first.toInstStr + "\n" + last.toInstStr

  override def arf[F<:Ordered[F]]:FwdPass[N,F] => Fact[C,F] => CMM#M[(Graph[N,C,C], Fact[C,F])] = { pass => f =>
    Graph.arfCat[N,C,O,C,F](first.arf[F](pass), last.arf[F](pass))(f)
  }
  override def arb[F<:Ordered[F]]:BwdPass[N,F] => Fact[C,F] => CMM#M[(Graph[N,C,C], Fact[C,F])] = { pass => f =>
    Graph.arbCat[N,C,O,C,F](first.arb[F](pass), last.arb[F](pass))(f)
  }
}

// Block decorated with fact
final case class DBlock[N<:NonLocal,E<:Status,X<:Status,F](val up:Block[N,E,X], val fact:F) extends Block[N,E,X]() {

  val e = up.e
  val x = up.x

  def map3[N1<:NonLocal](first:N=>N1, middle:N=>N1, last:N=>N1) = up.map3(first, middle, last)
  def toInstStr = up.toInstStr
  def toDotLabelRow = up.toDotLabelRow
  def succBlockIds = up.succBlockIds
  def blockId = up.blockId
  def backBiasBlock = up.backBiasBlock
  def frontBiasBlock = up.frontBiasBlock
  def cat[X1<:Status](that:Block[N,O,X1]) = {
    that match {
      case that0:DBlock[N,O,X1,F] => {
        DBlock[N,E,X1,F](up.cat(that0.up), fact)
      }
      case _ => throw new Exception()
    }
  }
  def node:N = up.node
  override def arf[F<:Ordered[F]]:FwdPass[N,F] => Fact[E,F] => CMM#M[(Graph[N,E,X], Fact[X,F])] = { pass => f =>
    up.arf[F](pass)(f)
  }
  override def arb[F<:Ordered[F]]:BwdPass[N,F] => Fact[E,F] => CMM#M[(Graph[N,E,X], Fact[X,F])] = { pass => f =>
    up.arb[F](pass)(f)
  }
  override def dropFact = up
}