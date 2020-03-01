
class val Frequency
  let _value:F64
  let _unit:String
  
  new val unit_Hz(value':F64) =>
    _value = value'
    _unit = "Hz"

  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun val string(): String =>
    _value.string() + " " + _unit

  fun val to_Hz():Frequency =>
    match _unit
    | "Hz" => this
    else this
    end
