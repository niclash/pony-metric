
class val Humidity
  let _value:F64
  let _unit:String
  
  new val unit_RH(value':F64) =>
    _value = value'
    _unit = "RH%"

  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun val string(): String =>
    _value.string() + " " + _unit

  fun val to_m_s2():Humidity =>
    match _unit
    | "RH%" => this
    else this
    end
