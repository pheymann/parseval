package parseval.json

sealed trait JsValue

case object JsNull extends JsValue

case object JsTrue extends JsValue

case object JsFalse extends JsValue

final case class JsString(value: String) extends JsValue

final case class JsNumber(value: Double) extends JsValue

final case class JsObject(fields: Map[String, JsValue]) extends JsValue

final case class JsArray(values: Array[JsValue]) extends JsValue
