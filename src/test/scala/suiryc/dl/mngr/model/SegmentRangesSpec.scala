package suiryc.dl.mngr.model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters._

class SegmentRangesSpec extends AnyWordSpec with Matchers {

  "SegmentRanges" should {
    "properly initialize its whole full range" in {
      val segmentRanges = new SegmentRanges(1234)
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(0, 1233))
    }

    "properly initialize its empty range" in {
      val segmentRanges = new SegmentRanges(1234, full = false)
      segmentRanges.getRanges.asScala shouldBe Nil
    }

    "handle removing outside ranges" in {
      val segmentRanges = new SegmentRanges(1000)
      segmentRanges.remove(SegmentRange(-1000, -1))
      segmentRanges.remove(SegmentRange(1000, 2000))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(0, 999))
    }

    "handle removing inside ranges" in {
      val segmentRanges = new SegmentRanges(1000)
      segmentRanges.remove(SegmentRange(-1000, 0))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(1, 999))
      segmentRanges.remove(SegmentRange(1, 2))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(3, 999))
      segmentRanges.remove(SegmentRange(999, 2000))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(3, 998))
      segmentRanges.remove(SegmentRange(997, 998))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(3, 996))
      segmentRanges.remove(SegmentRange(499, 499))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(3, 498), SegmentRange(500, 996))
      segmentRanges.remove(SegmentRange(100, 200))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(3, 99), SegmentRange(201, 498), SegmentRange(500, 996))
    }

    "handle clearing ranges" in {
      val segmentRanges = new SegmentRanges(1000)
      segmentRanges.clear()
      segmentRanges.getRanges.asScala shouldBe Nil
    }

    "handle adding outside ranges" in {
      val segmentRanges = new SegmentRanges(1000, full = false)
      segmentRanges.add(SegmentRange(-1000, -1))
      segmentRanges.add(SegmentRange(1000, 2000))
      segmentRanges.getRanges.asScala shouldBe Nil
    }

    "handle adding inside ranges" in {
      val segmentRanges = new SegmentRanges(1000, full = false)
      segmentRanges.add(SegmentRange(-1000, 0))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(0, 0))
      segmentRanges.add(SegmentRange(1, 2))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(0, 2))
      segmentRanges.add(SegmentRange(999, 2000))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(0, 2), SegmentRange(999, 999))
      segmentRanges.add(SegmentRange(997, 998))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(0, 2), SegmentRange(997, 999))
      segmentRanges.add(SegmentRange(499, 499))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(0, 2), SegmentRange(499, 499), SegmentRange(997, 999))
      segmentRanges.add(SegmentRange(100, 900))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(0, 2), SegmentRange(100, 900), SegmentRange(997, 999))
      segmentRanges.add(SegmentRange(3, 99))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(0, 900), SegmentRange(997, 999))
      segmentRanges.add(SegmentRange(950, 950))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(0, 900), SegmentRange(950, 950), SegmentRange(997, 999))
      segmentRanges.add(SegmentRange(-1000, 2000))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(0, 999))
    }

    "know whether a given position is inside" in {
      val segmentRanges = new SegmentRanges(1000)
      segmentRanges.remove(SegmentRange(100, 900))
      segmentRanges.contains(-1) shouldBe false
      segmentRanges.contains(0) shouldBe true
      segmentRanges.contains(99) shouldBe true
      segmentRanges.contains(100) shouldBe false
      segmentRanges.contains(900) shouldBe false
      segmentRanges.contains(901) shouldBe true
      segmentRanges.contains(999) shouldBe true
      segmentRanges.contains(1000) shouldBe false
    }

    "clone its ranges without sharing them" in {
      val segmentRanges = new SegmentRanges(1000)
      segmentRanges.remove(SegmentRange(100, 900))
      val segmentRanges2 = segmentRanges.clone()
      segmentRanges2.getRanges.asScala shouldBe segmentRanges.getRanges.asScala
      segmentRanges.remove(SegmentRange(50, 50))
      segmentRanges.getRanges.asScala shouldBe List(SegmentRange(0, 49), SegmentRange(51, 99), SegmentRange(901, 999))
      segmentRanges2.getRanges.asScala shouldBe List(SegmentRange(0, 99), SegmentRange(901, 999))
    }
  }

}
