class val Density
  let _value:F64
  let _unit:String
  
  new val unit_kg_m3(value':F64) =>
    _value = value'
    _unit = "kg/m³"

  new val unit_kg_dm3(value':F64) =>
    _value = value'
    _unit = "kg/dm³"

  new val unit_g_cm3(value':F64) =>
    _value = value'
    _unit = "g/cm³"

  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun val string(): String =>
    _value.string() + " " + _unit

  fun val to_kg_m3():Density =>
    match _unit
    | "kg/m³" => this
    | "kg/dm³" => unit_kg_m3(_value * 1000 )
    | "g/cm³" => unit_kg_m3(_value * 1000)
    else this
    end

  fun val to_kg_dm3():Density =>
    match _unit
    | "kg/m³" => unit_kg_dm3(_value / 1000 )
    | "kg/dm³" => this
    | "g/cm³" => unit_kg_dm3(_value)
    else this
    end

  fun val to_g_cm3():Density =>
    match _unit
    | "kg/m³" => unit_g_cm3(_value / 1000)
    | "kg/dm³" => unit_g_cm3(_value)
    | "g/cm³" => this
    else this
    end
