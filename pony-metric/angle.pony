
class val Angle
  let _value:F64
  let _unit:String
  
  new val unit_deg(value':F64) =>
    _value = value'
    _unit = "°"

  new val unit_rad(value':F64) =>
    _value = value'
    _unit = "rad"

  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun val string(): String =>
    _value.string() + " " + _unit

  fun val to_degrees():Angle =>
    match _unit
    | "°" => this
    | "rad" => unit_deg(_value * 57.29577951)
    else this
    end

  fun val to_radians():Angle =>
    match _unit
    | "°" => unit_rad(_value / 57.29577951)
    | "rad" => this
    else this
    end
