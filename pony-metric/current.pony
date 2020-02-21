
class val Current
  let _value:F64
  let _unit:String
  
  new val unit_A(value':F64) =>
    _value = value'
    _unit = "A"

  new val unit_kA(value':F64) =>
    _value = value'
    _unit = "kA"

  new val unit_mA(value':F64) =>
    _value = value'
    _unit = "mA"

  new val unit_uA(value':F64) =>
    _value = value'
    _unit = "µA"

  new val unit_nA(value':F64) =>
    _value = value'
    _unit = "nA"

  new val unit_pA(value':F64) =>
    _value = value'
    _unit = "pA"

  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun val string(): String =>
    _value.string() + " " + _unit

  fun val to_A():Current =>
    match _unit
    | "A" => this
    | "mA" => unit_A(_value * 1E-3)
    | "µA" => unit_A(_value * 1E-6)
    | "nA" => unit_A(_value * 1E-9)
    | "pA" => unit_A(_value * 1E-12)
    | "kA" => unit_A(_value * 1E3)
    else this
    end

  fun val to_kA():Current =>
    match _unit
    | "A" => unit_kA(_value * 1E-3)
    | "mA" => unit_kA(_value * 1E-6)
    | "µA" => unit_kA(_value * 1E-9)
    | "nA" => unit_kA(_value * 1E-12)
    | "pA" => unit_kA(_value * 1E-15)
    | "kA" => this
    else this
    end

  fun val to_mA():Current =>
    match _unit
    | "A" => unit_mA(_value * 1E3)
    | "mA" => this
    | "µA" => unit_mA(_value * 1E-3)
    | "nA" => unit_mA(_value * 1E-6)
    | "pA" => unit_mA(_value * 1E-9)
    | "kA" => unit_mA(_value * 1E6)
    else this
    end

  fun val to_uA():Current =>
    match _unit
    | "A" => unit_uA(_value * 1E6)
    | "mA" => unit_uA(_value * 1E3)
    | "µA" => this
    | "nA" => unit_uA(_value * 1E-3)
    | "pA" => unit_uA(_value * 1E-6)
    | "kA" => unit_uA(_value * 1E9)
    else this
    end

  fun val to_nA():Current =>
    match _unit
    | "A" => unit_nA(_value * 1E9)
    | "mA" => unit_nA(_value * 1E6)
    | "µA" => unit_nA(_value * 1E3)
    | "nA" => this
    | "pA" => unit_nA(_value * 1E-3)
    | "kA" => unit_nA(_value * 1E12)
    else this
    end

  fun val to_pA():Current =>
    match _unit
    | "A" => unit_pA(_value * 1E12)
    | "mA" => unit_pA(_value * 1E9)
    | "µA" => unit_pA(_value * 1E6)
    | "nA" => unit_pA(_value * 1E3)
    | "pA" => this
    | "kA" => unit_pA(_value * 1E15)
    else this
    end

