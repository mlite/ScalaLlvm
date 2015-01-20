package org.scalair.hoopl

import collection.immutable.{HashSet, HashMap}
import org.scalair.util.Logging

//import monad.{FuelMoandBuilderTrait, CheckingFuelMonadBuilder}
import monad.{CheckingFuelTmonadBuilderTrait, SimpleFuelMonadBuilder}
import org.scalair.monad.StateMonadBuilderTrait


/**
 * User: wangn
 * Date: 6/17/11
 */

object TypeDefines {
  type Unique = Int
  type BlockId = Unique
  type Fuel = Int
  type UniqueMap[V] = HashMap[Unique, V]
  type BlockIdMap[V] = HashMap[BlockId, V]
  type BlockIdSet = HashSet[BlockId]
  type Body[N<:NonLocal] = HashMap[BlockId, Block[N,C,C]]

  type FactBase[A] = BlockIdMap[A]

  def emptyBody[N <: NonLocal] = HashMap[BlockId,Block[N,C,C]]()
  def emptyLabelIdSet = HashSet[BlockId]()
  def singleton(x:BlockId) = emptyLabelIdSet + x
  def mapSingleton[A](k:BlockId, a:A) = HashMap[BlockId, A]() + (k -> a)

  def noFacts[T] = HashMap[BlockId, T]()

  var count = -1;
  def freshBlockId() = count -= 1; count // replace this later

  type HL[A] = List[A] => List[A]
  def consHL[A](a:A, as:HL[A]):HL[A] = { x:List[A] => as.apply(a::x) }
  def emptyHL[A]:HL[A] = identity[List[A]]_


  type Entries[e<:Status] = MaybeC[e, List[BlockId]]

  // O => Left
  // C => Right
  type Fact[+x<:Status,f] = Either[f,FactBase[f]]

  type DG[F,N<:NonLocal,E<:Status,X<:Status] = Graph[N,E,X]
  //type CMM = SimpleUniqueMonad.type //StateMonadUniqueCheckpointModule // CheckpointMonadModule
  type CMM = SimpleFuelMonadBuilder.type //CheckingFuelTmonadBuilderTrait //SimpleFuelMonad.type //SimpleUniqueMonad.type //StateMonadUniqueCheckpointModule // CheckpointMonadModule
  //type FMM = FuelMoandBuilderTrait


  implicit def labelsPtrList(l:List[BlockId]):LabelsPtr = {
    new LabelsPtr() {
      def targetBlockIds = l
    }
  }

  def compareList[T<:Ordered[T]](l1:List[T], l2:List[T]):Int = {
    (l1, l2) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => 1
      case (h1::t1, h2::t2) => {
        val r = h1.compare(h2)
        if (r == 0)
          compareList(t1, t2)
        else r
      }
    }
  }

  def compareListInt(l1:List[Int], l2:List[Int]):Int = {
    (l1, l2) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => 1
      case (h1::t1, h2::t2) => {
        val r = h1.compare(h2)
        if (r == 0)
          compareListInt(t1, t2)
        else r
      }
    }
  }

  def compareMap[K,V<:Ordered[V]](m1:Map[K,V], m2:Map[K,V]):Int = {
    -1
  }

  case class OrderedMap[K<:Ordered[K],V<:Ordered[V]](val map:Map[K,V]) extends Ordered[OrderedMap[K,V]] {
    def get(k:K) = map.get(k)
    def +(kv:(K,V)) = OrderedMap(map + kv)
    def compare(that:OrderedMap[K,V]):Int = 0
    def foldLeft[B](z:B)(op:(B,(K,V)) => B) = map.foldLeft(z)(op)
  }

  type SM[A] = StateMonadBuilderTrait#M[A]
}



trait HoopMonad extends Logging {
  val cmObj = SimpleFuelMonadBuilder// CheckingFuelMonadBuilder //SimpleFuelMonad//SimpleUniqueMonad
}
