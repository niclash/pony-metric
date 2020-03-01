
class val Mass
  let _value:F64
  let _unit:String
  
  new val unit_ug(value':F64) =>
    _value = value'
    _unit = "µg"

  new val unit_mg(value':F64) =>
    _value = value'
    _unit = "mg"

  new val unit_g(value':F64) =>
    _value = value'
    _unit = "g"

  new val unit_kg(value':F64) =>
    _value = value'
    _unit = "kg"

  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun val string(): String =>
    _value.string() + " " + _unit

  fun val to_kg():Mass =>
    match _unit
    | "µg" => unit_kg(_value / 1000000000)
    | "mg" => unit_kg(_value / 1000000)
    | "g" => unit_kg(_value / 1000)
    | "kg" => this
    else this
    end

  fun val to_g():Mass =>
    match _unit
    | "µg" => unit_g(_value / 1000000)
    | "mg" => unit_g(_value / 1000)
    | "g" => this
    | "kg" => unit_g(_value * 1000)
    else this
    end

  fun val to_mg():Mass =>
    match _unit
    | "µg" => unit_mg(_value / 1000)
    | "mg" => this
    | "g" => unit_mg(_value * 1000)
    | "kg" => unit_mg(_value * 1000000)
    else this
    end

  fun val to_ug():Mass =>
    match _unit
    | "µg" => this
    | "mg" => unit_ug(_value * 1000)
    | "g" => unit_ug(_value * 1000000)
    | "kg" => unit_ug(_value * 1000000000)
    else this
    end
