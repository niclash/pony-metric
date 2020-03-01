
class val Velocity
  let _value:F64
  let _unit:String
  
  new val unit_m_s(value':F64) =>
    _value = value'
    _unit = "m/s"
    
  new val unit_km_h(value':F64) =>
    _value = value'
    _unit = "km/h"
    
  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun val string(): String =>
    _value.string() + " " + _unit

  fun val to_m_s():Velocity =>
    match _unit
    | "m/s" => this
    | "km/h" => unit_m_s(_value / 3.6)
    else this
    end
    
  fun val to_km_h():Velocity =>
    match _unit
    | "m/s" => unit_km_h(_value * 3.6)
    | "km/h" => this
    else this
    end
