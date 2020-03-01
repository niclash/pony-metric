
class val Flow
  let _value:F64
  let _unit:String
  
  new val unit_m3_h(value':F64) =>
    _value = value'
    _unit = "m³/h"

  new val unit_m3_s(value':F64) =>
    _value = value'
    _unit = "m³/s"

  new val unit_l_s(value':F64) =>
    _value = value'
    _unit = "l/s"

  fun val value():F64 =>
    _value
    
  fun val unit():String =>
    _unit
    
  fun val string():String =>
    _value.string() + " " + _unit

  fun val to_m3_h():Flow =>
    match _unit
    | "m³/h" => this
    | "m³/s" => unit_m3_h(_value * 3600)
    | "l/s" => unit_m3_h(_value * 3.6)
    else this
    end

  fun val to_m3_s():Flow =>
    match _unit
    | "m³/h" => unit_m3_s(_value * 3600)
    | "m³/s" => this
    | "l/s" => unit_m3_s(_value * 1000)
    else this
    end

  fun val to_l_s():Flow =>
    match _unit
    | "m³/h" => unit_l_s(_value / 3.6)
    | "m³/s" => unit_l_s(_value / 1000)
    | "l/s" => this
    else this
    end
