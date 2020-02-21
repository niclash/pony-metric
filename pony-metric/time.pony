
class val Time
  let _value:F64
  let _unit:String
  
  new val unit_s(value':F64) =>
    _value = value'
    _unit = "s"
    
  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun val string(): String =>
    _value.string() + " " + _unit

  fun val to_s():Time =>
    match _unit
    | "s" => this
    else this
    end
