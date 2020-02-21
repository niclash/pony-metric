
class val Distance
  let _value:F64
  let _unit:String
  
  new val unit_m(value':F64) =>
    _value = value'
    _unit = "m"

  new val unit_dm(value':F64) =>
    _value = value'
    _unit = "dm"

  new val unit_cm(value':F64) =>
    _value = value'
    _unit = "cm"

  new val unit_mm(value':F64) =>
    _value = value'
    _unit = "mm"

  new val unit_km(value':F64) =>
    _value = value'
    _unit = "km"

  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun val string(): String =>
    _value.string() + " " + _unit

  fun val to_m():Distance =>
    match _unit
    | "m" => this
    | "dm" => unit_m(_value * 1E-1 )
    | "cm" => unit_m(_value * 1E-2 )
    | "mm" => unit_m(_value * 1E-3 )
    | "km" => unit_m(_value * 1E3 )
    else this
    end

  fun val to_dm():Distance =>
    match _unit
    | "m" => unit_dm(_value * 1E1 )
    | "dm" => this
    | "cm" => unit_dm(_value * 1E-1 )
    | "mm" => unit_dm(_value * 1E-2 )
    | "km" => unit_dm(_value * 1E4 )
    else this
    end

  fun val to_cm():Distance =>
    match _unit
    | "m" => unit_cm(_value * 1E2 )
    | "dm" => unit_cm(_value * 1E1 )
    | "cm" => this
    | "mm" => unit_cm(_value * 1E-1 )
    | "km" => unit_cm(_value * 1E5 )
    else this
    end

  fun val to_mm():Distance =>
    match _unit
    | "m" => unit_mm(_value * 1E3 )
    | "dm" => unit_mm(_value * 1E2 )
    | "cm" => unit_mm(_value * 1E1 )
    | "mm" => this
    | "km" => unit_mm(_value * 1E6 )
    else this
    end

  fun val to_km():Distance =>
    match _unit
    | "m" => unit_km(_value * 1E-3 )
    | "dm" => unit_km(_value * 1E-4 )
    | "cm" => unit_km(_value * 1E-5 )
    | "mm" => unit_km(_value * 1E-6 )
    | "km" => this
    else this
    end
