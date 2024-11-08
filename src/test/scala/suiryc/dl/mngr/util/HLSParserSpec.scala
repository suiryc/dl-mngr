package suiryc.dl.mngr.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class HLSParserSpec extends AnyWordSpec with Matchers {

  import HLSParser._

  "HLSParser" should {

    def testString(tag: Tag, expected: String): Unit = {
      // Directly test tag string representation
      tag.toString shouldBe expected
      // then again after copying tag (resets its raw representation)
      tag.copy().toString shouldBe expected
      ()
    }

    "handle non-tag line" in {
      HLSParser.parseTag("whatever") shouldBe None
      HLSParser.parseTag("  whatever  ") shouldBe None
    }

    "handle tag" in {
      // No value nor attributes.
      HLSParser.parseTag("#sometag") shouldBe
        Some(Tag("sometag", TagValue(), Nil))
      HLSParser.parseTag("#sometag:") shouldBe
        Some(Tag("sometag", TagValue(), Nil))

      // Only value.
      HLSParser.parseTag("#sometag:value") shouldBe
        Some(Tag("sometag", TagValue("value"), Nil))
      HLSParser.parseTag("""#sometag:"value"""") shouldBe
        Some(Tag("sometag", TagValue("value", quoted = true), Nil))
      HLSParser.parseTag("#sometag:value,") shouldBe
        Some(Tag("sometag", TagValue("value"), Nil))
      HLSParser.parseTag("""#sometag:"value",""") shouldBe
        Some(Tag("sometag", TagValue("value", quoted = true), Nil))

      // Only attributes.
      HLSParser.parseTag("#sometag:att=value") shouldBe
        Some(Tag("sometag", TagValue(), List(TagAttribute("att", TagValue("value")))))
      HLSParser.parseTag("""#sometag:att="value"""") shouldBe
        Some(Tag("sometag", TagValue(), List(TagAttribute("att", TagValue("value", quoted = true)))))
      HLSParser.parseTag("""#sometag:att1="value1",att2=value2,att3=value3,""") shouldBe
        Some(Tag("sometag", TagValue(), List(
          TagAttribute("att1", TagValue("value1", quoted = true)),
          TagAttribute("att2", TagValue("value2")),
          TagAttribute("att3", TagValue("value3"))
        )))

      // Value and attributes.
      HLSParser.parseTag("#sometag:value,att=value") shouldBe
        Some(Tag("sometag", TagValue("value"), List(TagAttribute("att", TagValue("value")))))
      HLSParser.parseTag("""#sometag:"value",att1="value1",att2=value2,att3=value3,""") shouldBe
        Some(Tag("sometag", TagValue("value", quoted = true), List(
          TagAttribute("att1", TagValue("value1", quoted = true)),
          TagAttribute("att2", TagValue("value2")),
          TagAttribute("att3", TagValue("value3"))
        )))
    }

    "handle finding tag attribute" in {
      val tag = Tag("name", TagValue("value"), List(
        TagAttribute("att1", TagValue("value1")),
        TagAttribute("att2", TagValue("value2", quoted = true))
      ))
      tag.findAttribute("att1") shouldBe Some(TagAttribute("att1", TagValue("value1")))
      tag.findAttribute("att2") shouldBe Some(TagAttribute("att2", TagValue("value2", quoted = true)))
      tag.findAttribute("att3") shouldBe None
    }

    "handle modifying tag attribute" in {
      val tag = Tag("name", TagValue("value"), List(
        TagAttribute("att1", TagValue("value1")),
        TagAttribute("att2", TagValue("value2", quoted = true))
      ))

      tag.changeAttribute("att1", TagValue("value2")) shouldBe Tag("name", TagValue("value"), List(
        TagAttribute("att1", TagValue("value2")),
        TagAttribute("att2", TagValue("value2", quoted = true))
      ))
      testString(tag.changeAttribute("att1", TagValue("value2")), """#name:value,att1=value2,att2="value2"""")

      tag.changeAttribute("att1", TagValue("value2", quoted = true)) shouldBe Tag("name", TagValue("value"), List(
        TagAttribute("att1", TagValue("value2", quoted = true)),
        TagAttribute("att2", TagValue("value2", quoted = true))
      ))
      testString(tag.changeAttribute("att1", TagValue("value2", quoted = true)), """#name:value,att1="value2",att2="value2"""")

      tag.changeAttribute("att2", TagValue("value2")) shouldBe Tag("name", TagValue("value"), List(
        TagAttribute("att1", TagValue("value1")),
        TagAttribute("att2", TagValue("value2"))
      ))
      testString(tag.changeAttribute("att2", TagValue("value2")), """#name:value,att1=value1,att2=value2""")

      tag.changeAttribute("att3", TagValue("value2")) shouldBe tag
      testString(tag.changeAttribute("att3", TagValue("value2")), tag.toString)
    }

  }

}
