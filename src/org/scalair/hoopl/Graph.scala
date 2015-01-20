package org.scalair.hoopl

import monad.VisitedStateMonadBuilder
import TypeDefines._
import collection.immutable.{HashSet, HashMap}
import org.scalair.monad.{StateTmonadBuilderTrait, IdentityMonadBuilder}
import org.scalair.util.Logging
import org.slf4j.MarkerFactory

/**
 * User: wangn
 * Date: 5/24/11
 */
object Graph extends HoopMonad {
  import TypeDefines._

  def nothingOentry[N<:NonLocal]() = NothingO[Block[N,O,C]]()
  def nothingOexit[N<:NonLocal]() = NothingO[Block[N,C,O]]()
  def justOentry[N<:NonLocal](x:Block[N,O,C]) = JustO[Block[N,O,C]](x)
  def justOexit[N<:NonLocal](x:Block[N,C,O]) = JustO[Block[N,C,O]](x)

  def mkGraph[N<:NonLocal,E<:Status,X<:Status](b:Block[N,E,X]):Graph[N,E,X] = b match {
    case b:BFirst[_] => gUnitCO[N](b)
    case b:BHead[_] => gUnitCO[N](b)
    case b:BMiddle[_] => gUnitOO[N](b)
    case b:BCat[_] => gUnitOO[N](b)
    case b:BLast[_] => gUnitOC[N](b)
    case b:BTail[_] => gUnitOC[N](b)
    case _ => throw new Exception()
  }

  def mkGraph[N<:NonLocal](hd:Block[N,O,C], list:List[Block[N,C,C]]):Graph[N,O,C] = {
    val hdGraph = gUnitOC[N](hd)
    val bodyGraph = listGraph[N](list)
    hdGraph.splice[C](bodyGraph)
  }

  def emptyGraph[N<:NonLocal]():Graph[N,O,O] = GNil[N]()

  def emptyBody[N<:NonLocal] = TypeDefines.emptyBody[N]

  def emptyCloseGraph[N<:NonLocal]():Graph[N,C,C] =
    GMany[N,C,C](nothingOentry[N](), emptyBody[N], nothingOexit[N]())

  def bodyGraph[N<:NonLocal](body:Body[N]):GMany[N,C,C] =
    GMany[N,C,C](nothingOentry[N](), body, nothingOexit[N]())

  def listGraph[N <:NonLocal](list:List[Block[N,C,C]]):GMany[N,C,C] = {
    val body = list.foldLeft(HashMap[BlockId, Block[N,C,C]]()) ((p, e) => p + (e.blockId -> e))
    bodyGraph[N](body)
  }

  def gUnitOO[N<:NonLocal](b:Block[N,O,O]):Graph[N,O,O] = GUnit[N](b)

  def gUnitOC[N<:NonLocal](b:Block[N,O,C]):Graph[N,O,C] =
    GMany[N,O,C](justOentry(b), emptyBody[N], nothingOexit[N]())

  def gUnitCO[N<:NonLocal](b:Block[N,C,O]):Graph[N,C,O] =
    GMany[N,C,O](nothingOentry[N], emptyBody[N], justOexit[N](b))

  def gUnitCC[N<:NonLocal](b:Block[N,C,C]):Graph[N,C,C] =
    GMany[N,C,C](nothingOentry[N], emptyBody[N] + (b.blockId -> b), nothingOexit[N])

  def dgUnitOO[N<:NonLocal](b:Block[N,O,O]):Graph[N,O,O] = GUnit[N](b)

  def dgUnitOC[N<:NonLocal](b:Block[N,O,C]):Graph[N,O,C] =
    GMany[N,O,C](justOentry(b), emptyBody[N], nothingOexit[N]())

  def dgUnitCO[N<:NonLocal](b:Block[N,C,O]):Graph[N,C,O] =
    GMany[N,C,O](nothingOentry[N], emptyBody[N], justOexit[N](b))

  def dgUnitCC[N<:NonLocal](b:Block[N,C,C]):Graph[N,C,C] =
    GMany[N,C,C](nothingOentry[N], emptyBody[N] + (b.blockId -> b), nothingOexit[N])

  def dgnilC[N<:NonLocal]():Graph[N,C,C] =
    GMany[N,C,C](nothingOentry[N], emptyBody[N], nothingOexit[N])



  // forward evaluation order
  def arfCat[N<:NonLocal,E<:Status,A<:Status,X<:Status,F](ft1:Fact[E,F]=>CMM#M[(Graph[N,E,A],Fact[A,F])],
                                                          ft2:Fact[O,F]=>CMM#M[(Graph[N,A,X],Fact[X,F])]):
  Fact[E,F]=>CMM#M[(Graph[N,E,X],Fact[X,F])] = { f =>
    ft1(f) >= { case (g1, f1) =>
      ft2(f1).flatMap { case (g2, f2) =>
        cmObj._return[(Graph[N,E,X],Fact[X,F])]((g1.splice[X](g2), f2))
      }
    }
  }

  // backward the evaluation order
  def arbCat[N<:NonLocal,E<:Status,A<:Status,X<:Status,F](ft1:Fact[E,F]=>CMM#M[(Graph[N,E,A],Fact[A,F])],
                                                          ft2:Fact[O,F]=>CMM#M[(Graph[N,A,X],Fact[X,F])]):
  Fact[E,F]=>CMM#M[(Graph[N,E,X],Fact[X,F])] = { f =>
    ft2(f) >= { case (g2, f2) =>
      ft1(f2).flatMap { case (g1, f1) =>
        cmObj._return[(Graph[N,E,X],Fact[X,F])]((g1.splice[X](g2), f1))
      }
    }
  }

  def analyzeAndRewriteFwd[E<:Status,X<:Status,N<:NonLocal,F<:Ordered[F]](graph:Graph[N,E,X],
                                                                          pass:FwdPass[N,F],
                                                                          entries:Entries[E],
                                                                          fact:Fact[E,F]):CMM#M[(Graph[N,E,X], FactBase[F], MaybeO[X,F])]
  =
  {
    graph.arf(pass)(entries)(fact) flatMap { case (rg, fout) =>
      val (g1, fb) = rg.normalize[F]
      graph.cmObj._return((g1,fb, g1.distinguishedExitFact[F](Right(fb))))
    }
  }

  def analyzeAndRewriteBwd[E<:Status,X<:Status,N<:NonLocal,F<:Ordered[F]](graph:Graph[N,E,X],
                                                                          pass:BwdPass[N,F],
                                                                          entries:Entries[E],
                                                                          fact:Fact[X,F]):CMM#M[(Graph[N,E,X], FactBase[F], MaybeO[E,F])]
  =
  {
    graph.arb(pass)(entries)(fact) flatMap { case (rg, fout) =>
      val (g1, fb) = rg.normalize[F]
      graph.cmObj._return((g1,fb, g1.distinguishedEntryFact[F](Right(fb))))
    }
  }
}

sealed abstract class Graph[N<:NonLocal,E<:Status,X<:Status](val e:E, val x:X)
  extends EXShape[E,X] with HoopMonad with Logging {
  val fixedpointMarker = MarkerFactory.getMarker("FIXEDPOINT")

  type HMImpl = Graph[N,E,X]
  type EXShapeImpl[e<:Status, x<:Status] = Graph[N,e,x]

  abstract class BlockResult[E<:Status, X<:Status]()

  case class NoBlock() extends BlockResult()
  case class BodyBlock(b:Block[N,C,C]) extends BlockResult[C,C]()
  case class ExitBlock(b:Block[N,C,O]) extends BlockResult[C,O]()


  def dotHead(name:String) = "digraph " + name + " {\n node [shape=plaintext];\n"

  def toDot(name:String):String
  def blockIdsDefined:BlockIdSet
  def blockIdsUsed:BlockIdSet
  final def externalEntryLabels:BlockIdSet = blockIdsDefined -- blockIdsUsed

  def entries:Entries[E]

  def appendNodeOO(n:N):Graph[N,E,O]
  def appendNodeOC(n:N):Graph[N,E,C]

  def prependNodeOO(n:N):Graph[N,O,X]
  def prependNodeCO(n:N):Graph[N,C,X]

  def splice[X1<:Status](that:Graph[N,X,X1]):Graph[N,E,X1]

  def postorderDfs:(Block[N,O,C], List[Block[N,C,C]])
  def preorderDfs:(Block[N,O,C], List[Block[N,C,C]])
  def inorderDom:(Block[N,O,C], List[Block[N,C,C]]) = throw new Exception()

  def toInstStr:String = {
    val (hd, tail) = postorderDfs
    tail.foldLeft(hd.toInstStr) ((p, e) => p + "\n" + e.toInstStr)
  }
  def map[N1<:NonLocal](f:Block[N,_,_]=>Block[N1,_,_]):Graph[N1,E,X]
  def mapInst[N1<:NonLocal](f:N=>N1):Graph[N1,E,X] = map[N1]((b:Block[N,_,_]) => b.map[N1](f))

  def orderMap[N1<:NonLocal](order: =>(Block[N,O,C], List[Block[N,C,C]]),
                             f:Block[N,_,_]=>Block[N1,_,_]):Graph[N1,O,C] = {
    val (hd, tail) = order
    val fe = f.asInstanceOf[Block[N,O,C]=>Block[N1,O,C]]
    val fb = f.asInstanceOf[Block[N,C,C]=>Block[N1,C,C]]
    val hd1 = fe(hd)
    val tail1 = tail.map(fb)
    Graph.mkGraph[N1](hd1, tail1)
  }

  def orderMapInst[N1<:NonLocal](order: =>(Block[N,O,C], List[Block[N,C,C]]), f:N=>N1):Graph[N1,O,C] =
    orderMap[N1](order, ((b:Block[N,_,_])=> b.map[N1](f)))


  def distinguishedEntryFact[F<:Ordered[F]]:Fact[E,F]=> MaybeO[E,F]
  def distinguishedExitFact[F<:Ordered[F]]:Fact[X,F]=>MaybeO[X,F]

  def arf[F<:Ordered[F]]:FwdPass[N,F]=>Entries[E]=>Fact[E,F]=>CMM#M[(Graph[N,E,X], Fact[X,F])]
  def arb[F<:Ordered[F]]:BwdPass[N,F]=>Entries[E]=>Fact[X,F]=>CMM#M[(Graph[N,E,X], Fact[X,F])]

  def facts[F<:Ordered[F]]:FactBase[F]
  def normalize[F<:Ordered[F]]:(Graph[N,E,X],FactBase[F]) = (map(_.dropFact), facts[F])
}

case class GNil[N<:NonLocal]() extends Graph[N,O,O](O(),O()) {

  def lookupBlock(l:BlockId) = NoBlock()
  def toDot(name:String) = "digraph {}"

  def entries = NothingC[List[BlockId]]()
  def blockIdsDefined:BlockIdSet = emptyLabelIdSet
  def blockIdsUsed:BlockIdSet = emptyLabelIdSet

  def appendNodeOO(n:N) = Graph.gUnitOO[N](BMiddle[N](n))
  def appendNodeOC(n:N) = Graph.gUnitOC[N](BLast[N](n))

  def prependNodeOO(n:N) = Graph.gUnitOO[N](BMiddle[N](n))
  def prependNodeCO(n:N) = Graph.gUnitCO[N](BFirst[N](n))

  def splice[X1<:Status](that:Graph[N,O,X1]) = that//.castEX[O,X1]

  def postorderDfs = throw new Exception()
  def preorderDfs = throw new Exception()
  def map[N1<:NonLocal](f:Block[N,_,_]=>Block[N1,_,_]) = GNil[N1]()

  def distinguishedEntryFact[F<:Ordered[F]]:Fact[O,F]=> MaybeO[O,F] = { fact => JustO[F](fact.left.get) }
  def distinguishedExitFact[F<:Ordered[F]]:Fact[O,F]=>MaybeO[O,F] = { fact => JustO[F](fact.left.get)}

  def arf[F<:Ordered[F]]:FwdPass[N,F]=>Entries[O]=>Fact[O,F]=>CMM#M[(Graph[N,O,O], Fact[O,F])] = {
    pass => entries => fact => cmObj._return[(Graph[N,O,O], Fact[O,F])]((GNil[N](), fact))
  }

  def arb[F<:Ordered[F]]:BwdPass[N,F]=>Entries[O]=>Fact[O,F]=>CMM#M[(Graph[N,O,O], Fact[O,F])] = {
    pass => entries => fact => cmObj._return[(Graph[N,O,O], Fact[O,F])]((GNil[N](), fact))
  }

  def facts[F<:Ordered[F]] = noFacts[F]
}

case class GUnit[N<:NonLocal](m:Block[N,O,O]) extends Graph[N,O,O](O(),O()) {

  def lookupBlock(l:BlockId) = NoBlock()
  def toDot(name:String) = dotHead(name) + m.toDot + "\n}"

  def blockIdsDefined:BlockIdSet = emptyLabelIdSet
  def blockIdsUsed:BlockIdSet = emptyLabelIdSet

  def entries = NothingC[List[BlockId]]()

  def appendNodeOO(n:N) =
    Graph.gUnitOO[N](BCat[N](m, BMiddle[N](n)))

  def appendNodeOC(n:N) =
    Graph.gUnitOC[N](m.addToRight(BLast[N](n)))

  def prependNodeOO(n:N) =
    Graph.gUnitOO[N](BCat[N](BMiddle[N](n), m))

  def prependNodeCO(n:N) =
    Graph.gUnitCO[N](m.addToLeft(BFirst[N](n)))


  def splice[X1<:Status](that:Graph[N,O,X1]) = that match {
    case that0:GNil[N] => this.castEX[O,X1]
    case that0:GUnit[N] => GUnit[N](m.cat[O](that0.m))
    case GMany(thatentry, thatbody, thatexit) if (thatentry.status == O()) =>
      GMany[N,O,X1](Graph.justOentry[N](m.cat[C](thatentry.get)), thatbody.asInstanceOf[Body[N]], thatexit)
    case _ => throw new Exception()
  }

  def postorderDfs = throw new Exception()
  def preorderDfs = throw new Exception()

  def map[N1<:NonLocal](f:Block[N,_,_]=>Block[N1,_,_]) = {
    val fo = f.asInstanceOf[Block[N,O,O] => Block[N1,O,O]]
    GUnit[N1](fo(m))
  }

  def distinguishedEntryFact[F<:Ordered[F]]:Fact[O,F]=> MaybeO[O,F] = { fact => JustO[F](fact.left.get) }
  def distinguishedExitFact[F<:Ordered[F]]:Fact[O,F]=> MaybeO[O,F] = { fact => JustO[F](fact.left.get) }

  def arf[F<:Ordered[F]]:FwdPass[N,F]=>Entries[O]=>Fact[O,F]=>CMM#M[(Graph[N,O,O], Fact[O,F])] = {
    pass => entries => fact => m.arf[F](pass)(fact)
  }

  def arb[F<:Ordered[F]]:BwdPass[N,F]=>Entries[O]=>Fact[O,F]=>CMM#M[(Graph[N,O,O], Fact[O,F])] = {
    pass => entries => fact => m.arb[F](pass)(fact)
  }

  def facts[F<:Ordered[F]] = noFacts[F]
}



case class GMany[N<:NonLocal,E<:Status,X<:Status](entry:MaybeO[E,Block[N,O,C]],
                                                  body:Body[N],
                                                  exit:MaybeO[X,Block[N,C,O]])
  extends Graph[N,E,X](entry.status,exit.status) {

  def entries = entry.status match {
    case O() => NothingC[List[BlockId]]().asInstanceOf[Entries[E]]
    case C() => throw new Exception()
  }

  private def addBlock(b:Block[N,C,C]):Body[N] = {
    val k = b.blockId()
    if (body.isDefinedAt(k)) throw new Exception()
    else body + (b.blockId() -> b)
  }

  def bodyList():List[(BlockId, Block[N,C,C])] = body.foldLeft(List[(BlockId, Block[N,C,C])]())((p, e) => e::p)

  private def bodyMap[N1<:NonLocal](f:Block[N,C,C]=>Block[N1,C,C]):Body[N1] =
    body.map(a => {val b1 = f(a._2); (b1.blockId, b1)})

  private def map3[N1<:NonLocal](fe:Block[N,O,C] => Block[N1,O,C],
                                 fb:Block[N,C,C]=>Block[N1,C,C],
                                 fx:Block[N,C,O]=>Block[N1,C,O]):Graph[N1,E,X] =
    GMany[N1,E,X](entry.map[Block[N1,O,C]](fe), bodyMap[N1](fb), exit.map[Block[N1,C,O]](fx))

  def map[N1<:NonLocal](f:Block[N,_,_]=>Block[N1,_,_]):Graph[N1,E,X] = {
    val fe = f.asInstanceOf[Block[N,O,C]=> Block[N1,O,C]]
    val fb = f.asInstanceOf[Block[N,C,C]=> Block[N1,C,C]]
    val fx = f.asInstanceOf[Block[N,C,O]=> Block[N1,C,O]]
    map3[N1](fe, fb, fx)
  }

  def lookupBlock(lbl:BlockId) = exit.status match {
    case O() if (exit.get.blockId == lbl) => ExitBlock(exit.get)
    case _ => {
      body.get(lbl) match {
        case Some(b) => BodyBlock(b)
        case None => NoBlock()
      }
    }
  }

  def blockIdsDefined:BlockIdSet = {
    def addEntry(labels:BlockIdSet, t:(BlockId, Block[N,C,C])):BlockIdSet = { val (label, _) = t; labels + label }
    exit.status match {
      case O() => body.foldLeft(singleton(exit.get.blockId))(addEntry)
      case _ => emptyLabelIdSet
    }
  }
  def blockIdsUsed:BlockIdSet = emptyLabelIdSet

  def appendNodeOO(n:N) = exit.status match {
    case O() => GMany[N,E,O](entry, body, Graph.justOexit[N](BHead[N](exit.get, BMiddle[N](n))))
    case _ => throw new Exception()
  }

  def appendNodeOC(n:N) = exit.status match {
    case O() => {
      val body1 = addBlock(BClosed[N](exit.get, BLast[N](n)))
      GMany[N,E,C](entry, body1, Graph.nothingOexit[N])
    }
    case _ => throw new Exception()
  }

  def prependNodeOO(n:N) = entry.status match {
    case O() =>
      GMany[N,O,X](Graph.justOentry(BTail[N](BMiddle[N](n), entry.get)), body, exit)
    case _ => throw new Exception()
  }

  def prependNodeCO(n:N) = entry.status match {
    case O() => {
      val body1 = addBlock(BClosed[N](BFirst[N](n), entry.get))
      GMany[N,C,X](Graph.nothingOentry[N], body1, exit)
    }
    case _ => throw new Exception()
  }

  def splice[X1<:Status](that:Graph[N,X,X1]):Graph[N,E,X1] = {
    that match {
      case that0:GNil[N] => this.asInstanceOf[Graph[N,E,X1]]
      case that0:GUnit[N] => exit.status match {
        case O() => {
          val ret = GMany[N,E,O](entry, body, Graph.justOexit[N](exit.get.cat[O](that0.m)))
          ret.asInstanceOf[GMany[N,E,X1]]
        }
        case _ => throw new Exception()
      }
      case that0:GMany[N,X,X1] => {
        (exit.status, that0.entry.status) match {
          case (O(), O()) => {
            val x1 = exit.get
            val e2 = that0.entry.get
            val b1 = addBlock(x1.cat(e2))
            GMany[N,E,X1](entry, (b1 ++ that0.body), that0.exit)
          }
          case (C(), C()) => GMany[N,E,X1](entry, (body ++ that0.body), that0.exit)
          case _ => throw new Exception()
        }
      }
      case _ => throw new Exception()
    }
  }


  def toDot(name:String) =  {
    def renderEdges(src:Int)(q:String, x:Int):String = q + "\n" + "block" + src + "->block" + x

    /*
    val entryGraph = entry.status match {
      case O() => {
        val t = entry.get
        val edges = t.succBlockIds().foldLeft("")(renderEdges(t.blockId))
        t.toDot + "\n" + edges
      }
      case _ => ""
    }
    */
    val bodyGraph = body.foldLeft(""/*entryGraph*/)((p, e)=> {val edges = e._2.succBlockIds().foldLeft("")(renderEdges(e._1))
      p + "\n" + e._2.toDot + "\n" + edges})
    val exitGraph = exit.status match {
      case O() => exit.get.toDot
      case _ => ""
    }
    dotHead(name) + bodyGraph + "\n" + exitGraph + "\n}"
  }


  def dfs(order:(Block[N,O,C], BlockIdSet)=>List[Block[N,C,C]]):(Block[N,O,C], List[Block[N,C,C]]) =
    entry.status match {
      case O() =>  val t = entry.get; (t, order(t, emptyLabelIdSet))
      case _ => throw new Exception ("")
    }

  def getChildren[L <: LabelsPtr](block:L):List[Block[N,C,C]] = {
    def add_id (rst:List[Block[N,C,C]], id:BlockId):List[Block[N,C,C]] = body.get(id) match {
      case Some(b) => b::rst
      case None => rst
    }
    block.targetBlockIds().foldLeft(List[Block[N,C,C]]()) ((p, e) => add_id(p, e))
  }

  private def postorder_dfs_from_except[B<:LabelsPtr](b:B, visited:BlockIdSet):List[Block[N,C,C]] = {
    def vnode(block:Block[N,C,C], cont:(List[Block[N,C,C]], BlockIdSet)=>List[Block[N,C,C]],
              acc:List[Block[N,C,C]], visited:BlockIdSet):List[Block[N,C,C]] = {
      def id = block.blockId()
      if (visited.contains(id)) cont(acc, visited)
      else {
        def cont1(acc:List[Block[N,C,C]], visited:BlockIdSet):List[Block[N,C,C]] = cont(block::acc, visited)
        vchildren(getChildren(block), cont1, acc, visited + id)
      }
    }
    def vchildren(children:List[Block[N,C,C]], cont:(List[Block[N,C,C]], BlockIdSet)=>List[Block[N,C,C]],
                  acc:List[Block[N,C,C]], visited:BlockIdSet):List[Block[N,C,C]] = {
      def next(children:List[Block[N,C,C]])(acc:List[Block[N,C,C]], visited:BlockIdSet):List[Block[N,C,C]] =
        children match {
          case List() => cont(acc, visited)
          case b::bs => vnode(b, next(bs), acc, visited)
        }
      next(children)(acc, visited)
    }
    vchildren(getChildren(b), ((acc, _visitied) => acc), List(), visited)
  }

  private def preorder_dfs_from_except[B<:LabelsPtr](b:B, visited:BlockIdSet):List[Block[N,C,C]] = {
    def children(lst:List[Block[N,C,C]]):VisitedStateMonadBuilder.M[HL[Block[N,C,C]]] = lst match {
      case Nil => VisitedStateMonadBuilder._return[HL[Block[N,C,C]]](emptyHL[Block[N,C,C]])
      case b::bs =>
        VisitedStateMonadBuilder._liftM2({
          x:Function1[List[Block[N,C,C]], List[Block[N,C,C]]] =>
            y:Function1[List[Block[N,C,C]], List[Block[N,C,C]]] =>
              x.compose(y) })(visit(b))(children(bs))
    }
    def visit(b:Block[N,C,C]):VisitedStateMonadBuilder.M[HL[Block[N,C,C]]] = {
      VisitedStateMonadBuilder.marked(b.blockId) flatMap { already =>
        if (already) VisitedStateMonadBuilder._return[HL[Block[N,C,C]]](emptyHL[Block[N,C,C]])
        else {
          VisitedStateMonadBuilder.mark(b.blockId) flatMap { _ =>
            children(getChildren(b)) flatMap { bs => VisitedStateMonadBuilder._return[HL[Block[N,C,C]]](consHL(b, bs)) }
          }
        }
      }
    }
    children(getChildren(b)).runState(visited)(List[Block[N,C,C]]())._1(List[Block[N,C,C]]())
  }


  def postorderDfs = dfs(postorder_dfs_from_except)
  def preorderDfs = dfs(preorder_dfs_from_except)

  def postorder_dfs_from[B<:LabelsPtr](b:B):List[Block[N,C,C]] = postorder_dfs_from_except(b, HashSet())
  def forwardBlockList[B<:LabelsPtr](entry:B):List[Block[N,C,C]] = postorder_dfs_from(entry)
  def backwardBlockList[B<:LabelsPtr](entry:B):List[Block[N,C,C]] = forwardBlockList(entry).reverse


  def distinguishedEntryFact[F<:Ordered[F]]:Fact[E,F]=> MaybeO[E,F] = { fact =>
    entry.status match {
      case O() => JustO[F](fact.left.get).asInstanceOf[MaybeO[E,F]]
      case _  => NothingO[F]().asInstanceOf[MaybeO[E,F]]
    }
  }

  def distinguishedExitFact[F<:Ordered[F]]:Fact[X,F]=> MaybeO[X,F] = { fact =>
    exit.status match {
      case O() => JustO[F](fact.left.get).asInstanceOf[MaybeO[X,F]]
      case _  => NothingO[F]().asInstanceOf[MaybeO[X,F]]
    }
  }

  private def arfBody[F<:Ordered[F]]:Pass[F] => List[BlockId]=>Fact[C,F]=>CMM#M[(Graph[N,C,C], Fact[C,F])] = {
    pass=>entries=>init_fbase=>
      val blocks = forwardBlockList(labelsPtrList(entries))
      fixpoint(pass)(blocks)(init_fbase)
  }

  private def arbBody[F<:Ordered[F]]:Pass[F] => List[BlockId]=>Fact[C,F]=>CMM#M[(Graph[N,C,C], Fact[C,F])] = {
    pass=>entries=>init_fbase=>
      val blocks = backwardBlockList(labelsPtrList(entries))
      fixpoint(pass)(blocks)(init_fbase)
  }

  private def arfEbcat[F<:Ordered[F]]:FwdPass[N,F] => Entries[E] => Fact[E,F] =>CMM#M[(Graph[N,E,C], Fact[C,F])] = {
    pass => entries =>
      (entries.status, entry.status) match {
        case (O(), O()) => {
          Graph.arfCat[N,O,C,C,F](entry.get.arf[F](pass),
            arfBody[F](pass)(entry.get.succBlockIds)).asInstanceOf[Fact[E,F]=>CMM#M[(Graph[N,E,C], Fact[C,F])]]
        }
        case (C(), C()) => arfBody[F](pass)(entries.get).asInstanceOf[Fact[E,F]=>CMM#M[(Graph[N,E,C], Fact[C,F])]]
        case _ => throw new Exception()
    }
  }

  private def arfExit[F<:Ordered[F]]:FwdPass[N,F]=> Fact[C,F]=>CMM#M[(Graph[N,C,X], Fact[X,F])] = { pass =>
    exit.status match {
      case O() => exit.get.arfx(pass).asInstanceOf[Fact[C,F]=>CMM#M[(Graph[N,C,X], Fact[X,F])]]
      case C() => { fb:Fact[C,F] => cmObj._return((Graph.dgnilC[N]().castEX[C,X], fb)) }
    }
  }

  private def arbExit[F<:Ordered[F]]:BwdPass[N,F]=> Fact[C,F]=>CMM#M[(Graph[N,C,X], Fact[X,F])] = { pass =>
    exit.status match {
      case O() => exit.get.arbx(pass).asInstanceOf[Fact[C,F]=>CMM#M[(Graph[N,C,X], Fact[X,F])]]
      case C() => { fb:Fact[C,F] => cmObj._return((Graph.dgnilC[N]().castEX[C,X], fb)) }
    }
  }

  private def arbEbcat[F<:Ordered[F]]:BwdPass[N,F] => Entries[E] => Fact[C,F] =>CMM#M[(Graph[N,E,C], Fact[E,F])] = {
    pass => entries =>
      (entries.status, entry.status) match {
        case (O(), O()) => {
          Graph.arbCat[N,O,C,C,F](entry.get.arb[F](pass),
            arbBody[F](pass)(entry.get.succBlockIds)).asInstanceOf[Fact[E,F]=>CMM#M[(Graph[N,E,C], Fact[C,F])]]
        }
        case (C(), C()) => arbBody[F](pass)(entries.get).asInstanceOf[Fact[E,F]=>CMM#M[(Graph[N,E,C], Fact[C,F])]]
        case _ => throw new Exception()
    }
  }




  def arf[F<:Ordered[F]]:FwdPass[N,F]=>Entries[E]=>Fact[E,F]=>CMM#M[(Graph[N,E,X], Fact[X,F])]={
    pass => entries => Graph.arfCat[N,E,C,X,F](arfEbcat[F](pass)(entries), arfExit[F](pass))
  }

  def arb[F<:Ordered[F]]:BwdPass[N,F]=>Entries[E]=>Fact[X,F]=>CMM#M[(Graph[N,E,X], Fact[X,F])]={
    pass => entries => Graph.arbCat[N,E,C,X,F](arbEbcat[F](pass)(entries), arbExit[F](pass))
  }

  def exitFacts[F<:Ordered[F]]:FactBase[F] = exit.status match {
    case O() => mapSingleton(exit.get.blockId, exit.get.asInstanceOf[DBlock[N,_,_,F]].fact)
    case C() => noFacts[F]
  }

  def bodyFacts[F<:Ordered[F]]:FactBase[F] = body.foldLeft(noFacts[F]) { (p,e) =>
    p + (e._1 -> e._2.asInstanceOf[DBlock[N,_,_,F]].fact)
  }
  def facts[F<:Ordered[F]] = bodyFacts[F] ++ exitFacts[F]

  private def loop[F<:Ordered[F]]:Pass[F]=>List[(Block[N,C,C], List[BlockId])]=> FactBase[F] => CMM#M[TxFactBase[N,F]] =
  { pass => tagged_blocks=> fbase =>
    logger.debug(fixedpointMarker, "[loop.tagged_blocks] {}", tagged_blocks)
    logger.debug(fixedpointMarker, "[loop.fbase] {}" + fbase)
    def tx_blocks[F<:Ordered[F]]:List[(Block[N,C,C], List[BlockId])] =>TxFactBase[N,F] => CMM#M[TxFactBase[N,F]] =
    {
      lst => tx_fb =>
        lst match {
          case Nil => cmObj._return[TxFactBase[N,F]](tx_fb)
          case (blk, in_lbls)::tail => {
            logger.debug(fixedpointMarker, "[loop.current] {}", blk)
            logger.debug(fixedpointMarker, "[loop.in_lbs] {}", in_lbls)
            logger.debug(fixedpointMarker, "[loop.tx_fb] {}", tx_fb)
            blk.tx_block[F](pass.asInstanceOf[Pass[F]])(in_lbls)(tx_fb) >= tx_blocks[F](tail)
          }
        }
    }
    val init_tx = TxFactBase[N,F](fbase, Graph.dgnilC[N](), NoChange, HashSet())
    cmObj.checkpoint >= { s =>
      tx_blocks(tagged_blocks)(init_tx) >= { tx_fb =>
        tx_fb.cha match {
          case NoChange => cmObj._return(tx_fb)
          case SomeChange => cmObj.restart(s) flatMap { _ =>
            loop(pass)(tagged_blocks)(tx_fb.fbase)
          }
        }
      }
    }
  }

  private def fixpoint[F<:Ordered[F]]:Pass[F]=>List[Block[N,C,C]]=>Fact[C,F]=>CMM#M[(Graph[N,C,C], Fact[C,F])] = {
    pass => bodys => init_fbase =>
      logger.debug(fixedpointMarker, "[fixpoint.bodys] {}", bodys)
      logger.debug(fixedpointMarker, "[fixpoint.init_fbase] {}", init_fbase)
      val tagged_blocks:List[(Block[N,C,C], List[BlockId])] = {
        bodys.map { blk =>
          if (pass.isFwd) (blk, List(blk.blockId))
          else (blk, blk.succBlockIds)
        }
      }
      val tagged_block_set = tagged_blocks.foldLeft(Set[BlockId]()) { (p,e) => p + e._1.blockId }
      loop(pass)(tagged_blocks)(init_fbase.right.get) >= { tx_fb =>
      // The successors of the graph are the labels (blockIds)
      // for which we have facts and which are not in the blocks
      // of the graph
        val fbase1 = tx_fb.fbase.foldLeft(HashMap[BlockId, F]()) { (p, e) => if (tagged_block_set.contains(e._1)) p else p + e }
        cmObj._return[(Graph[N,C,C], Fact[C,F])]((tx_fb.rg, Right(fbase1)))
      }
  }
}