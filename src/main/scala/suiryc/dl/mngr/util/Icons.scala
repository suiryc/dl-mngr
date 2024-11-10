package suiryc.dl.mngr.util

import javafx.scene.Node
import suiryc.scala.javafx.scene.Graphics
import suiryc.scala.javafx.scene.Graphics._

object Icons extends IconBuilders {

  private val PREFIX = "icon-"

  // Font Awesome SVG icons.
  // Font Awesome by Dave Gandy - http://fontawesome.io
  // License: http://fontawesome.io/license

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/angle-double-down?f=classic&s=solid
  val angleDoubleDown: Builder = iconBuilder(s"${PREFIX}angle-double-down") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgWidth = 320, svgHeight = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M143 256.3L7 120.3c-9.4-9.4-9.4-24.6 0-33.9l22.6-22.6c9.4-9.4 24.6-9.4 33.9 0l96.4 96.4 96.4-96.4c9.4-9.4 24.6-9.4 33.9 0L313 86.3c9.4 9.4 9.4 24.6 0 33.9l-136 136c-9.4 9.5-24.6 9.5-34 .1zm34 192l136-136c9.4-9.4 9.4-24.6 0-33.9l-22.6-22.6c-9.4-9.4-24.6-9.4-33.9 0L160 352.1l-96.4-96.4c-9.4-9.4-24.6-9.4-33.9 0L7 278.3c-9.4 9.4-9.4 24.6 0 33.9l136 136c9.4 9.5 24.6 9.5 34 .1z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/angle-down?f=classic&s=solid
  val angleDown: Builder = iconBuilder(s"${PREFIX}angle-down") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgWidth = 320, svgHeight = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M143 352.3L7 216.3c-9.4-9.4-9.4-24.6 0-33.9l22.6-22.6c9.4-9.4 24.6-9.4 33.9 0l96.4 96.4 96.4-96.4c9.4-9.4 24.6-9.4 33.9 0l22.6 22.6c9.4 9.4 9.4 24.6 0 33.9l-136 136c-9.2 9.4-24.4 9.4-33.8 0z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/angle-double-up?f=classic&s=solid
  val angleDoubleUp: Builder = iconBuilder(s"${PREFIX}angle-double-up") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgWidth = 320, svgHeight = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M177 255.7l136 136c9.4 9.4 9.4 24.6 0 33.9l-22.6 22.6c-9.4 9.4-24.6 9.4-33.9 0L160 351.9l-96.4 96.4c-9.4 9.4-24.6 9.4-33.9 0L7 425.7c-9.4-9.4-9.4-24.6 0-33.9l136-136c9.4-9.5 24.6-9.5 34-.1zm-34-192L7 199.7c-9.4 9.4-9.4 24.6 0 33.9l22.6 22.6c9.4 9.4 24.6 9.4 33.9 0l96.4-96.4 96.4 96.4c9.4 9.4 24.6 9.4 33.9 0l22.6-22.6c9.4-9.4 9.4-24.6 0-33.9l-136-136c-9.2-9.4-24.4-9.4-33.8 0z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/angle-up?f=classic&s=solid
  val angleUp: Builder = iconBuilder(s"${PREFIX}angle-up") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgWidth = 320, svgHeight = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M177 159.7l136 136c9.4 9.4 9.4 24.6 0 33.9l-22.6 22.6c-9.4 9.4-24.6 9.4-33.9 0L160 255.9l-96.4 96.4c-9.4 9.4-24.6 9.4-33.9 0L7 329.7c-9.4-9.4-9.4-24.6 0-33.9l136-136c9.4-9.5 24.6-9.5 34-.1z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/bug?f=classic&s=solid
  val bug: Builder = iconBuilder(s"${PREFIX}bug") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgSize = 512, targetSvgSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M512 288.9c-.5 17.4-15.2 31.1-32.7 31.1H424v16c0 21.9-4.9 42.6-13.6 61.1l60.2 60.2c12.5 12.5 12.5 32.8 0 45.3-12.5 12.5-32.8 12.5-45.3 0l-54.7-54.7C345.9 468 314.4 480 280 480V236c0-6.6-5.4-12-12-12h-24c-6.6 0-12 5.4-12 12v244c-34.4 0-65.9-12-90.6-32.1l-54.7 54.7c-12.5 12.5-32.8 12.5-45.3 0-12.5-12.5-12.5-32.8 0-45.3l60.2-60.2C92.9 378.6 88 357.9 88 336v-16H32.7C15.2 320 .5 306.3 0 288.9-.5 270.8 14 256 32 256h56v-58.7l-46.6-46.6c-12.5-12.5-12.5-32.8 0-45.3 12.5-12.5 32.8-12.5 45.3 0L141.3 160h229.5l54.6-54.6c12.5-12.5 32.8-12.5 45.3 0 12.5 12.5 12.5 32.8 0 45.3L424 197.3V256h56c18 0 32.5 14.8 32 32.9zM257 0c-61.9 0-112 50.1-112 112h224C369 50.1 318.9 0 257 0z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/check-square?f=classic&s=solid
  val checkSquare: Builder = iconBuilder(s"${PREFIX}check-square") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgWidth = 448, svgHeight = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M400 480H48c-26.5 0-48-21.5-48-48V80c0-26.5 21.5-48 48-48h352c26.5 0 48 21.5 48 48v352c0 26.5-21.5 48-48 48zm-204.7-98.1l184-184c6.2-6.2 6.2-16.4 0-22.6l-22.6-22.6c-6.2-6.2-16.4-6.2-22.6 0L184 302.7l-70.1-70.1c-6.2-6.2-16.4-6.2-22.6 0l-22.6 22.6c-6.2 6.2-6.2 16.4 0 22.6l104 104c6.2 6.3 16.4 6.3 22.6 0z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/cog?f=classic&s=solid
  val cog: Builder = iconBuilder(s"${PREFIX}cog") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgSize = 512, targetSvgSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M487.4 315.7l-42.6-24.6c4.3-23.2 4.3-47 0-70.2l42.6-24.6c4.9-2.8 7.1-8.6 5.5-14-11.1-35.6-30-67.8-54.7-94.6-3.8-4.1-10-5.1-14.8-2.3L380.8 110c-17.9-15.4-38.5-27.3-60.8-35.1V25.8c0-5.6-3.9-10.5-9.4-11.7-36.7-8.2-74.3-7.8-109.2 0-5.5 1.2-9.4 6.1-9.4 11.7V75c-22.2 7.9-42.8 19.8-60.8 35.1L88.7 85.5c-4.9-2.8-11-1.9-14.8 2.3-24.7 26.7-43.6 58.9-54.7 94.6-1.7 5.4 .6 11.2 5.5 14L67.3 221c-4.3 23.2-4.3 47 0 70.2l-42.6 24.6c-4.9 2.8-7.1 8.6-5.5 14 11.1 35.6 30 67.8 54.7 94.6 3.8 4.1 10 5.1 14.8 2.3l42.6-24.6c17.9 15.4 38.5 27.3 60.8 35.1v49.2c0 5.6 3.9 10.5 9.4 11.7 36.7 8.2 74.3 7.8 109.2 0 5.5-1.2 9.4-6.1 9.4-11.7v-49.2c22.2-7.9 42.8-19.8 60.8-35.1l42.6 24.6c4.9 2.8 11 1.9 14.8-2.3 24.7-26.7 43.6-58.9 54.7-94.6 1.5-5.5-.7-11.3-5.6-14.1zM256 336c-44.1 0-80-35.9-80-80s35.9-80 80-80 80 35.9 80 80-35.9 80-80 80z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/download?f=classic&s=solid
  val download: Builder = iconBuilder(s"${PREFIX}download") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgSize = 512, targetSvgSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M216 0h80c13.3 0 24 10.7 24 24v168h87.7c17.8 0 26.7 21.5 14.1 34.1L269.7 378.3c-7.5 7.5-19.8 7.5-27.3 0L90.1 226.1c-12.6-12.6-3.7-34.1 14.1-34.1H192V24c0-13.3 10.7-24 24-24zm296 376v112c0 13.3-10.7 24-24 24H24c-13.3 0-24-10.7-24-24V376c0-13.3 10.7-24 24-24h146.7l49 49c20.1 20.1 52.5 20.1 72.6 0l49-49H488c13.3 0 24 10.7 24 24zm-124 88c0-11-9-20-20-20s-20 9-20 20 9 20 20 20 20-9 20-20zm64 0c0-11-9-20-20-20s-20 9-20 20 9 20 20 20 20-9 20-20z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/eraser?f=classic&s=solid
  val eraser: Builder = iconBuilder(s"${PREFIX}eraser") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgSize = 512, targetSvgSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M497.9 273.9c18.7-18.7 18.7-49.1 0-67.9l-160-160c-18.7-18.7-49.1-18.7-67.9 0l-256 256c-18.7 18.7-18.7 49.1 0 67.9l96 96A48 48 0 0 0 144 480h356c6.6 0 12-5.4 12-12v-40c0-6.6-5.4-12-12-12H355.9l142.1-142.1zm-302.6-62.6l137.4 137.4L265.4 416H150.6l-80-80 124.7-124.7z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/exclamation-triangle?f=classic&s=solid
  val exclamationTriangle: Builder = iconBuilder(s"${PREFIX}exclamation-triangle") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgWidth = 576, svgHeight = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M569.5 440C588 472 564.8 512 527.9 512H48.1c-36.9 0-60-40.1-41.6-72L246.4 24c18.5-32 64.7-32 83.2 0l239.9 416zM288 354c-25.4 0-46 20.6-46 46s20.6 46 46 46 46-20.6 46-46-20.6-46-46-46zm-43.7-165.3l7.4 136c.3 6.4 5.6 11.3 12 11.3h48.5c6.4 0 11.6-5 12-11.3l7.4-136c.4-6.9-5.1-12.7-12-12.7h-63.4c-6.9 0-12.4 5.8-12 12.7z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/hourglass?f=classic&s=regular
  val hourglass: Builder = iconBuilder(s"${PREFIX}hourglass") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgWidth = 384, svgHeight = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M368 48h4c6.6 0 12-5.4 12-12V12c0-6.6-5.4-12-12-12H12C5.4 0 0 5.4 0 12v24c0 6.6 5.4 12 12 12h4c0 80.6 32.2 165.8 97.2 208C47.9 298.4 16 383.9 16 464h-4c-6.6 0-12 5.4-12 12v24c0 6.6 5.4 12 12 12h360c6.6 0 12-5.4 12-12v-24c0-6.6-5.4-12-12-12h-4c0-80.6-32.2-165.8-97.2-208C336.1 213.6 368 128.1 368 48zM64 48h256c0 101.6-57.3 184-128 184S64 149.6 64 48zm256 416H64c0-101.6 57.3-184 128-184s128 82.4 128 184z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/info-circle?f=classic&s=solid
  val infoCircle: Builder = iconBuilder(s"${PREFIX}info-circle") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgSize = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M256 8C119 8 8 119.1 8 256c0 137 111 248 248 248s248-111 248-248C504 119.1 393 8 256 8zm0 110c23.2 0 42 18.8 42 42s-18.8 42-42 42-42-18.8-42-42 18.8-42 42-42zm56 254c0 6.6-5.4 12-12 12h-88c-6.6 0-12-5.4-12-12v-24c0-6.6 5.4-12 12-12h12v-64h-12c-6.6 0-12-5.4-12-12v-24c0-6.6 5.4-12 12-12h64c6.6 0 12 5.4 12 12v100h12c6.6 0 12 5.4 12 12v24z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/minus?f=classic&s=solid
  val minus: Builder = iconBuilder(s"${PREFIX}minus") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgWidth = 448, svgHeight = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, snapToPixel = true, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M416 208H32c-17.7 0-32 14.3-32 32v32c0 17.7 14.3 32 32 32h384c17.7 0 32-14.3 32-32v-32c0-17.7-14.3-32-32-32z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/save?f=classic&s=regular
  val save: Builder = iconBuilder(s"${PREFIX}save") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgWidth = 448, svgHeight = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M433.9 129.9l-83.9-83.9A48 48 0 0 0 316.1 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V163.9a48 48 0 0 0 -14.1-33.9zM272 80v80H144V80h128zm122 352H54a6 6 0 0 1 -6-6V86a6 6 0 0 1 6-6h42v104c0 13.3 10.7 24 24 24h176c13.3 0 24-10.7 24-24V83.9l78.2 78.2a6 6 0 0 1 1.8 4.2V426a6 6 0 0 1 -6 6zM224 232c-48.5 0-88 39.5-88 88s39.5 88 88 88 88-39.5 88-88-39.5-88-88-88zm0 128c-22.1 0-40-17.9-40-40s17.9-40 40-40 40 17.9 40 40-17.9 40-40 40z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/sign-out-alt?f=classic&s=solid
  val signOut: Builder = iconBuilder(s"${PREFIX}sign-out") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgSize = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M497 273L329 441c-15 15-41 4.5-41-17v-96H152c-13.3 0-24-10.7-24-24v-96c0-13.3 10.7-24 24-24h136V88c0-21.4 25.9-32 41-17l168 168c9.3 9.4 9.3 24.6 0 34zM192 436v-40c0-6.6-5.4-12-12-12H96c-17.7 0-32-14.3-32-32V160c0-17.7 14.3-32 32-32h84c6.6 0 12-5.4 12-12V76c0-6.6-5.4-12-12-12H96c-53 0-96 43-96 96v192c0 53 43 96 96 96h84c6.6 0 12-5.4 12-12z")
    )
  }

  // Font Awesome 6.
  // https://fontawesome.com/icons/square-xmark?f=classic&s=solid
  val squareXmark: Builder = iconBuilder(s"${PREFIX}square-xmark") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgWidth = 448, svgHeight = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M64 32C28.7 32 0 60.7 0 96L0 416c0 35.3 28.7 64 64 64l320 0c35.3 0 64-28.7 64-64l0-320c0-35.3-28.7-64-64-64L64 32zm79 143c9.4-9.4 24.6-9.4 33.9 0l47 47 47-47c9.4-9.4 24.6-9.4 33.9 0s9.4 24.6 0 33.9l-47 47 47 47c9.4 9.4 9.4 24.6 0 33.9s-24.6 9.4-33.9 0l-47-47-47 47c-9.4 9.4-24.6 9.4-33.9 0s-9.4-24.6 0-33.9l47-47-47-47c-9.4-9.4-9.4-24.6 0-33.9z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/stop?f=classic&s=solid
  val stop: Builder = iconBuilder(s"${PREFIX}stop") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgWidth = 448, svgHeight = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M400 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V80c0-26.5-21.5-48-48-48z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/sync?f=classic&s=solid
  val sync: Builder = iconBuilder(s"${PREFIX}sync") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgSize = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M440.7 12.6l4 82.8A247.2 247.2 0 0 0 255.8 8C134.7 8 33.9 94.9 12.3 209.8A12 12 0 0 0 24.1 224h49.1a12 12 0 0 0 11.7-9.3 175.9 175.9 0 0 1 317-56.9l-101.5-4.9a12 12 0 0 0 -12.6 12v47.4a12 12 0 0 0 12 12H500a12 12 0 0 0 12-12V12a12 12 0 0 0 -12-12h-47.4a12 12 0 0 0 -12 12.6zM255.8 432a175.6 175.6 0 0 1 -146-77.8l101.8 4.9a12 12 0 0 0 12.6-12v-47.4a12 12 0 0 0 -12-12H12a12 12 0 0 0 -12 12V500a12 12 0 0 0 12 12h47.4a12 12 0 0 0 12-12.6l-4.2-82.6A247.2 247.2 0 0 0 255.8 504c121.1 0 221.9-86.9 243.6-201.8a12 12 0 0 0 -11.8-14.2h-49.1a12 12 0 0 0 -11.7 9.3A175.9 175.9 0 0 1 255.8 432z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/tachometer-alt?f=classic&s=solid
  val tachometer: Builder = iconBuilder(s"${PREFIX}tachometer") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgWidth = 576, svgHeight = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M288 32C128.9 32 0 160.9 0 320c0 52.8 14.3 102.3 39.1 144.8 5.6 9.6 16.3 15.2 27.4 15.2h443c11.1 0 21.8-5.6 27.4-15.2C561.8 422.3 576 372.8 576 320c0-159.1-128.9-288-288-288zm0 64c14.7 0 26.6 10.1 30.3 23.7-1.1 2.3-2.6 4.2-3.5 6.7l-9.2 27.7c-5.1 3.5-11 6-17.6 6-17.7 0-32-14.3-32-32S270.3 96 288 96zM96 384c-17.7 0-32-14.3-32-32s14.3-32 32-32 32 14.3 32 32-14.3 32-32 32zm48-160c-17.7 0-32-14.3-32-32s14.3-32 32-32 32 14.3 32 32-14.3 32-32 32zm246.8-72.4l-61.3 184C343.1 347.3 352 364.5 352 384c0 11.7-3.4 22.6-8.9 32H232.9c-5.5-9.5-8.9-20.3-8.9-32 0-33.9 26.5-61.4 59.9-63.6l61.3-184c4.2-12.6 17.7-19.5 30.4-15.2 12.6 4.2 19.4 17.8 15.2 30.4zm14.7 57.2l15.5-46.6c3.5-1.3 7.1-2.2 11.1-2.2 17.7 0 32 14.3 32 32s-14.3 32-32 32c-11.4 0-20.9-6.3-26.6-15.2zM480 384c-17.7 0-32-14.3-32-32s14.3-32 32-32 32 14.3 32 32-14.3 32-32 32z")
    )
  }

  // Font Awesome 5.
  // https://fontawesome.com/v5/icons/undo?f=classic&s=solid
  val undo: Builder = iconBuilder(s"${PREFIX}undo") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgSize = 512, targetSvgSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M212.3 224.3H12c-6.6 0-12-5.4-12-12V12C0 5.4 5.4 0 12 0h48c6.6 0 12 5.4 12 12v78.1C117.8 39.3 184.3 7.5 258.2 8c136.9 1 246.4 111.6 246.2 248.5C504 393.3 393.1 504 256.3 504c-64.1 0-122.5-24.3-166.5-64.2-5.1-4.6-5.3-12.6-.5-17.4l34-34c4.5-4.5 11.7-4.7 16.4-.5C170.8 415.3 211.6 432 256.3 432c97.3 0 176-78.7 176-176 0-97.3-78.7-176-176-176-58.5 0-110.3 28.5-142.3 72.3h98.3c6.6 0 12 5.4 12 12v48c0 6.6-5.4 12-12 12z")
    )
  }

  val icons: Map[String, Builder] = List(bug, checkSquare, cog, download, eraser,
    exclamationTriangle, infoCircle, minus, save, signOut, squareXmark, stop, sync, tachometer, undo).map { builder =>
    builder().params.styleClass.head -> builder
  }.toMap

  def setIcons(node: Node): Unit = {
    Graphics.setIcons(node, _.startsWith(PREFIX), Icons.icons.get(_).map(_().pane))
  }

}
