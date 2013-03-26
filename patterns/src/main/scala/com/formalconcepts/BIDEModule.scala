package com.formalconcepts

/**---------------------------------------------
 * User: Michael Thomas
 * Date: 26/03/2013
 * Time: 12:18
 *---------------------------------------------*/
trait BIDEModule {

  import scala.annotation._

  // Pattern mining single-event sequences
  trait BIDE {
    type Ident
    type Elem

    type Pattern = Sequence
    type Sequence = Vector[Elem]

    type Context = Vector[(Ident, Pattern)]
    case class Concept(extent: Set[Ident], intent: ClosedPattern)

    type ClosedPattern = Vector[Sequence]

    case class SeqInfo(id: Int, nElem: Int)
    type SeqPos = Int

    type PseudoSeq = (SeqInfo, List[SeqPos])
    type PDB = Vector[PseudoSeq]

    final val initSeqPos = -1

    def pseudo(ctxt: Context): PDB =
      (ctxt,Stream.from(0)).zipped.
        map({ case ((_, sq), idx) => (SeqInfo(idx, sq.length), List(initSeqPos)) }).toVector

    import scala.math.{ min, max }

    @inline
    def nextItem(si: SeqInfo): (SeqPos => SeqPos) = { m => min(m + 1, si.nElem) }

    @inline
    def prevItem(si: SeqInfo): (SeqPos => SeqPos) = { m => max(-1, m - 1) }

    @inline
    def endOfSeq(si: SeqInfo): SeqPos = si.nElem

    @inline
    def isEndOfSeq(si: SeqInfo): SeqPos => Boolean = { _ >= si.nElem }

    @inline
    def extend(pfx: Sequence)(x: Elem): Sequence = pfx :+ x

    @inline
    def getSequence(ctxt: Context, si: SeqInfo): Sequence = ctxt(si.id)._2

    @inline
    def lastElem(pfx: Sequence) = pfx(pfx.length - 1)

    def firstOccAfter(context: Context, si: SeqInfo, x: Elem): SeqPos => Option[SeqPos] = {
      val (sq: Sequence) = getSequence(context, si)

      { (n: Int) =>
        val pos: SeqPos = sq.indexWhere((x == _), n + 1)
        if (pos > -1) Some(pos) else None
      }
    }

    def lastOccAfter(context: Context, si: SeqInfo, elem: Elem): SeqPos => Option[SeqPos] = {
      def f = firstOccAfter(context, si, elem)

      @tailrec
      def aux(sp: SeqPos, sp2: Option[SeqPos]): SeqPos = sp2 match {
        case Some(sp) => aux(sp, f(sp))
        case _ => sp
      }

      (f(_) match {
        case None => None
        case Some(sp) => Some(aux(sp, f(sp)))
      })
    }

    def unfoldPrefix(context: Context, pfx: Sequence, si: SeqInfo, sp: SeqPos) = {
      val sq = getSequence(context, si)
      @tailrec
      def aux2(x: Elem, sp: SeqPos): SeqPos = if (sq(sp) == x) sp else aux2(x, prevItem(si)(sp))
      // is the sequence is empty, it didn't contain the prefix we want
      if (isEndOfSeq(si)(sp)) Stream.empty
      else pfx.reverse.toStream match {
        case (x #:: xs) =>
          val i = (sp, x, None: Option[Elem])
          xs.scanLeft(i)({ case ((sp, y, _), x) => (aux2(x, prevItem(si)(sp)), x, Some(y)) })
      }
    }


    def lastInLast(context: Context, pfx: Sequence)(si: SeqInfo, sp: SeqPos) = {
      lastOccAfter(context, si, lastElem(pfx))(sp) match {
        case Some(li) => unfoldPrefix(context, pfx, si, li) // there is another occurrence of the last itemset after the current sp
        case _ => unfoldPrefix(context, pfx, si, sp) // the current sp corresponds to the last occurrence
      }
    }



    def lastInFirst(context: Context, pfx: Sequence)(si: SeqInfo, sp: SeqPos) = {
      unfoldPrefix(context, pfx, si, sp)
    }

    def period(context: Context, pdb: PDB, pfx: Sequence, f: ((Context, Sequence) => (SeqInfo, SeqPos) => Stream[(SeqPos, Elem, Option[Elem])])) =
      pdb.map({
        case (si, sps) =>
          val sq = getSequence(context, si)
          sps.tail.toStream.
            zip(f(context, pfx)(si, sps.head)).
            map {
            case (x, (y, i, j)) => sq.slice(max(0, x + 1), min(y, sq.length)).toSet
          }
      }).filter(!_.isEmpty)

    def maxPeriod(sdb: Context, pdb: PDB, pfx: Sequence) = period(sdb, pdb.filter({ case (si, sps) => !isEndOfSeq(si)(sps.head) }), pfx, lastInLast)

    def semiMaxPeriod(sdb: Context, pdb: PDB, pfx: Sequence) = {
      pdb.filter({ case (si, sps) => !isEndOfSeq(si)(sps.head) }).map({
        case (si, sps) => sps.sliding(2).toStream.map({
          case (x :: y :: _) => sdb(si.id)._2.slice(y + 1, x).toSet
          case _ => Set.empty[Elem]
        })
      })
    }

    @tailrec
    final def hasExtension(xss: Vector[Stream[Set[Elem]]]): Boolean = {
      if (xss.exists(_.isEmpty)) false
      else xss.map({ case (x #:: xs) => (x, xs) }).unzip match {
        case (ys, _) if (ys.isEmpty) => false
        case (ys, _) if (!ys.reduce(_ intersect _).isEmpty) => true
        case (_, yss) => hasExtension(yss)
      }
    }

    @inline
    def backScanPrune(context: Context, pdb: PDB, pfx: Sequence) =
      hasExtension(semiMaxPeriod(context, pdb, pfx))

    @inline
    def hasBackwardExtension(context: Context, pdb: PDB, pfx: Sequence) =
      hasExtension(maxPeriod(context, pdb, pfx))

    import scala.collection.mutable.{ HashMap, Set => MSet }

    def scan(ctxt: Context, pdb: PDB): HashMap[Elem, MSet[Int]] = {
      val lut = new HashMap[Elem, MSet[Int]]()

      pdb.foreach({
        case (si, sps) =>
          getSequence(ctxt, si).slice(sps.head + 1, si.nElem).foreach { x =>
            if (lut.contains(x)) lut(x) = lut(x) + si.id
            else lut += ((x, MSet(si.id)))
          }
      })

      lut
    }

    @inline
    def hasForwardExtension(fs: Stream[(Elem, MSet[Int])], sup: Int) = fs.exists({ case (_, rs) => rs.size == sup })

    def project(ctxt: Context)(pdb: PDB)(x: Elem): PDB =
      pdb.collect({
        case (si, sps) if (!isEndOfSeq(si)(sps.head)) =>
          firstOccAfter(ctxt, si, x)(sps.head) match {
            case Some(sp2) => (si, sp2 :: sps)
            case _ => (si, (endOfSeq(si)) :: sps)
          }
      })


  }
}
