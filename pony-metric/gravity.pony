
class val Gravity
  let _value:F64
  let _unit:String
  
  new val unit_s(value':F64) =>
    _value = value'
    _unit = "m/s²"

  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun val string(): String =>
    _value.string() + " " + _unit

  fun val to_m_s2():Gravity =>
    match _unit
    | "m/s²" => this
    else this
    end
