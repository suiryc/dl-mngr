package suiryc.dl.mngr.util


object HLSParser {

  // See RFC 8216.
  // We only handle what we need.
  // In particular, this code don't properly handle all kind of tags, only
  // those we do need to process and sometimes replace.

  def parseTag(line: String): Option[Tag] = {
    Option.when(line.startsWith("#")) {
      val parser = new TagParser(line)
      val name = parser.parseTagName()
      val value = parser.parseTagValue()
      val attributes = parser.parseAttributes()
      Tag(name, value, attributes).setRaw(line)
    }
  }

  case class Tag(name: String, value: TagValue, attributes: List[TagAttribute]) {

    private var raw: Option[String] = None

    def setRaw(raw: String): Tag = {
      this.raw = Some(raw)
      this
    }

    def findAttribute(name: String): Option[TagAttribute] = {
      attributes.find(_.name == name)
    }

    def changeAttribute(name: String, value: TagValue): Tag = {
      copy(
        attributes = attributes.map { attribute =>
          if (attribute.name == name) {
            TagAttribute(attribute.name, value)
          } else {
            attribute
          }
        }
      )
    }

    override def toString: String = {
      raw.getOrElse {
        var s = s"#$name"
        if (value.value.nonEmpty || attributes.nonEmpty) s = s"$s:$value"
        if (value.value.nonEmpty && attributes.nonEmpty) s = s"$s,"
        if (attributes.nonEmpty) s = s"$s${attributes.mkString(",")}"
        raw = Some(s)
        s
      }
    }

  }

  case class TagValue(
    value: String = "",
    quoted: Boolean = false
  ) {
    override def toString: String = {
      if (quoted) s""""$value"""" else value
    }
  }

  case class TagAttribute(
    name: String,
    value: TagValue
  ) {
    override def toString: String = s"$name=$value"
  }

  private class TagParser(private var data: String) {

    private def skipOffset(off: Int, skipNext: Int = 1): String = {
      val v = data.substring(0, off).trim()
      data = data.substring(math.min(off + skipNext, data.length)).trim()
      v
    }

    def parseTagName(): String = {
      var name = ""
      // Skip leading '#'.
      this.skipOffset(0)
      val idx = data.indexOf(':')
      if (idx < 0) {
        name = data
        data = ""
      } else {
        name = skipOffset(idx)
      }
      name
    }

    def parseTagValue(): TagValue = {
      // Consider the tag value may be a quoted string too.
      parseQuotedString().getOrElse {
        var v = ""
        // Tag main value (non-attributes) ends with first ',' unless it actually
        // is an attribute (contains '=').
        val idxComma = data.indexOf(',')
        val idxEqual = data.indexOf('=')
        if (idxComma < 0) {
          // At best there is one attribute.
          if (idxEqual < 0) {
            // Only tag value.
            v = data
            data = ""
          } else {
            // There is no value, but one attribute.
          }
        } else {
          // There may be at least one attribute.
          if ((idxEqual < 0) || (idxEqual > idxComma)) {
            // There is a value and zero or more attribute(s) following.
            v = skipOffset(idxComma)
          } else {
            // There is no value, and at least one attribute.
          }
        }
        TagValue(v)
      }
    }

    def parseAttributes(): List[TagAttribute] = {
      @scala.annotation.tailrec
      def loop(attributes: List[TagAttribute]): List[TagAttribute] = {
        if (data.nonEmpty) {
          var attName = ""
          var attValue = TagValue()
          val idxComma = data.indexOf(',')
          val idxEqual = data.indexOf('=')
          if (idxComma < 0) {
            if (idxEqual < 0) {
              // value = 'XXX'
              // Should not happen for normal attributes.
              // May happen for some tags, e.g. 'EXTINF' which value is a
              // duration followed by a comma and optional title.
              // Anyway, consider the remaining value as attribute name.
              attName = data
              data = ""
            } else {
              // value = 'XXX=XXX'
              attName = skipOffset(idxEqual)
              attValue = parseAttributeValue()
              data = ""
            }
          } else {
            if ((idxEqual < 0) || (idxEqual > idxComma)) {
              // value = XXX,...
              // Should not happen, but consider the next part as attribute
              // name.
              attName = skipOffset(idxComma)
            } else {
              // value = XXX=XXX,...
              attName = skipOffset(idxEqual)
              attValue = parseAttributeValue()
            }
          }
          loop(attributes :+ TagAttribute(attName, attValue))
        } else {
          attributes
        }
      }
      loop(Nil)
    }

    private def parseAttributeValue(): TagValue = {
      parseQuotedString().getOrElse {
        val idxComma = data.indexOf(',')
        TagValue(skipOffset(if (idxComma < 0) data.length else idxComma))
      }
    }

    private def parseQuotedString(): Option[TagValue] = {
      Option.when(data.nonEmpty && (data.head == '"')) {
        // Quoted string ends on next quote.
        // Skip the quote.
        skipOffset(0)
        val idx = data.indexOf('"')
        if (idx < 0) {
          // No ending quote.
          // Should not happen: consider remaining text the value.
          skipOffset(data.length)
        } else {
          val s = skipOffset(idx)
          if (data.nonEmpty) {
            if (data.head != ',') {
              // There are extra data after quote and before next parameter.
              // Should not happen: ignore the extra data.
              val idxComma = data.indexOf(',')
              if (idxComma < 0) {
                data = ""
              } else {
                skipOffset(idxComma)
              }
            } else {
              // Skip ','.
              skipOffset(0)
            }
          }
          s
        }
      }.map { s =>
        TagValue(value = s, quoted = true)
      }
    }

  }

}
