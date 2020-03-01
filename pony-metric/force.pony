
class val Force
  let _value:F64
  let _unit:String
  
  new val unit_N(value':F64) =>
    _value = value'
    _unit = "N"

  new val unit_kN(value':F64) =>
    _value = value'
    _unit = "kN"

  new val unit_MN(value':F64) =>
    _value = value'
    _unit = "MN"

  fun val value():F64 =>
    _value
    
  fun val unit():String =>
    _unit
    
  fun val string():String =>
    _value.string() + " " + _unit

  fun val to_N():Force =>
    match _unit
    | "N" => this
    | "kN" => unit_N(_value * 1000)
    | "MN" => unit_N(_value * 1000000)
    else this
    end

  fun val to_kN():Force =>
    match _unit
    | "N" => unit_kN(_value * 1000)
    | "kN" => this
    | "MN" => unit_kN(_value * 1000)
    else this
    end

  fun val to_MN():Force =>
    match _unit
    | "N" => unit_MN(_value / 1000000)
    | "kN" => unit_MN(_value / 1000)
    | "MN" => this
    else this
    end

