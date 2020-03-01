/* Copyright 2020 Niclas Hedhman

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

class val Power
  let _value:F64
  let _unit:String

  new val unit_nW(value':F64) =>
    _value=value'
    _unit = "W"
  
  new val unit_uW(value':F64) =>
    _value=value'
    _unit = "µW"

// TODO: Complain abou the pony compiler until it becomes case sensitive    
//   new val unit_mW(value':F64) =>
  new val unit_milliWatt(value':F64) =>
    _value=value'
    _unit = "mW"
  
  new val unit_W(value':F64) =>
    _value=value'
    _unit = "W"
  
  new val unit_kW(value':F64) =>
    _value=value'
    _unit = "kW"
  
  new val unit_MW(value':F64) =>
    _value=value'
    _unit = "MW"
  
  new val unit_GW(value':F64) =>
    _value=value'
    _unit = "GW"
  
  new val unit_TW(value':F64) =>
    _value=value'
    _unit = "TW"
  
  new val unit_kWh_y(value':F64) =>
    _value=value'
    _unit = "kWh/y"
    
  new val unit_MWh_y(value':F64) =>
    _value=value'
    _unit = "MWh/y"

  new val unit_GWh_y(value':F64) =>
    _value=value'
    _unit = "GWh/y"
  
  new val unit_TWh_y(value':F64) =>
    _value=value'
    _unit = "TWh/y"
  
  new val unit_J_s(value':F64) =>
    _value=value'
    _unit = "J/s"
  
  new val unit_kJ_s(value':F64) =>
    _value=value'
    _unit = "kJ/s"
  
  new val unit_MJ_s(value':F64) =>
    _value=value'
    _unit = "MJ/s"
  
  new val unit_GJ_s(value':F64) =>
    _value=value'
    _unit = "GJ/s"
  
  new val unit_Nm_s(value':F64) =>
    _value=value'
    _unit = "Nm/s"
  
  new val parse(t:String)? =>
    let pos = t.find(" ")?
    _value = t.substring(0,pos).f64()?
    _unit = t.substring(pos+1)
    match _unit
    | "Nm/s" => None
    | "J/s" => None
    | "kJ/s" => None
    | "MJ/s" => None
    | "GJ/s" => None
    | "nW" => None
    | "µW" => None
    | "mW" => None
    | "W" => None
    | "kW" => None
    | "MW" => None
    | "GW" => None
    | "TW" => None
    | "kWh/y" => None
    | "MWh/y" => None
    | "GWh/y" => None
    | "TWh/y" => None
    else error
    end
  
  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun val string(): String =>
    _value.string() + " " + _unit

  fun val to_nW() =>
    match _unit
    | "Nm/s" => unit_nW(_value * 1E9)
    | "J/s" => unit_nW(_value * 1E9)
    | "kJ/s" => unit_nW(_value * 1E12)
    | "MJ/s" => unit_nW(_value * 1E15)
    | "GJ/s" => unit_nW(_value * 1E18)
    | "nW" => this
    | "µW" => unit_nW(_value * 1E3)
    | "mW" => unit_nW(_value * 1E6)
    | "W" => unit_nW(_value * 1E9)
    | "kW" => unit_nW(_value * 1E12)
    | "MW" => unit_nW(_value * 1E15)
    | "GW" => unit_nW(_value * 1E18)
    | "TW" => unit_nW(_value * 1E21)
    | "kWh/y" => unit_nW(_value / 8.76E-9)
    | "MWh/y" => unit_nW(_value / 8.76E-12)
    | "GWh/y" => unit_nW(_value / 8.76E-15)
    | "TWh/y" => unit_nW(_value / 8.76E-18)
    else this
    end

  
  fun val to_uW() =>
    match _unit
    | "Nm/s" => unit_uW(_value * 1E6)
    | "J/s" => unit_uW(_value * 1E6)
    | "kJ/s" => unit_uW(_value * 1E9)
    | "MJ/s" => unit_uW(_value * 1E12)
    | "GJ/s" => unit_uW(_value * 1E15)
    | "nW" => unit_uW(_value / 1000)
    | "µW" => this
    | "mW" => unit_uW(_value * 1E3)
    | "W" => unit_uW(_value * 1E6)
    | "kW" => unit_uW(_value * 1E9)
    | "MW" => unit_uW(_value * 1E12)
    | "GW" => unit_uW(_value * 1E15)
    | "TW" => unit_uW(_value * 1E18)
    | "kWh/y" => unit_uW(_value / 8.76E-6)
    | "MWh/y" => unit_uW(_value / 8.76E-9)
    | "GWh/y" => unit_uW(_value / 8.76E-12)
    | "TWh/y" => unit_uW(_value / 8.76E-15)
    else this
    end

// TODO: Complain abou the pony compiler until it becomes case sensitive    
//   fun val to_mW() =>
  fun val to_milliWatt() =>
    match _unit
    | "Nm/s" => unit_milliWatt(_value * 1E3)
    | "J/s" => unit_milliWatt(_value * 1E3)
    | "kJ/s" => unit_milliWatt(_value * 1E6)
    | "MJ/s" => unit_milliWatt(_value * 1E9)
    | "GJ/s" => unit_milliWatt(_value * 1E12)
    | "nW" => unit_milliWatt(_value / 1E6)
    | "µW" => unit_milliWatt(_value / 1E3)
    | "mW" => this
    | "W" => unit_milliWatt(_value * 1E3)
    | "kW" => unit_milliWatt(_value * 1E6)
    | "MW" => unit_milliWatt(_value * 1E9)
    | "GW" => unit_milliWatt(_value * 1E12)
    | "TW" => unit_milliWatt(_value * 1E15)
    | "kWh/y" => unit_milliWatt(_value / 8.76E-3)
    | "MWh/y" => unit_milliWatt(_value / 8.76E-6)
    | "GWh/y" => unit_milliWatt(_value / 8.76E-9)
    | "TWh/y" => unit_milliWatt(_value / 8.76E-12)
    else this
    end
  
  fun val to_W() =>
    match _unit
    | "Nm/s" => unit_W(_value)
    | "J/s" => unit_W(_value)
    | "kJ/s" => unit_W(_value * 1E3)
    | "MJ/s" => unit_W(_value * 1E6)
    | "GJ/s" => unit_W(_value * 1E9)
    | "nW" => unit_W(_value / 1E9)
    | "µW" => unit_W(_value / 1E6)
    | "mW" => unit_W(_value / 1E3)
    | "W" => this
    | "kW" => unit_W(_value * 1E3)
    | "MW" => unit_W(_value * 1E6)
    | "GW" => unit_W(_value * 1E9)
    | "TW" => unit_W(_value * 1E12)
    | "kWh/y" => unit_W(_value / 8.76)
    | "MWh/y" => unit_W(_value / 8.76E-3)
    | "GWh/y" => unit_W(_value / 8.76E-6)
    | "TWh/y" => unit_W(_value / 8.76E-9)
    else this
    end
  
  fun val to_kW() =>
    match _unit
    | "Nm/s" => unit_kW(_value / 1E3)
    | "J/s" => unit_kW(_value / 1E3)
    | "kJ/s" => unit_kW(_value)
    | "MJ/s" => unit_kW(_value * 1E3)
    | "GJ/s" => unit_kW(_value * 1E6)
    | "nW" => unit_kW(_value / 1E12)
    | "µW" => unit_kW(_value / 1E9)
    | "mW" => unit_kW(_value / 1E6)
    | "W" => unit_kW(_value / 1E3)
    | "kW" => this
    | "MW" => unit_kW(_value * 1E3)
    | "GW" => unit_kW(_value * 1E6)
    | "TW" => unit_kW(_value * 1E9)
    | "kWh/y" => unit_kW(_value / 8.76E3)
    | "MWh/y" => unit_kW(_value / 8.76)
    | "GWh/y" => unit_kW(_value / 8.76E-3)
    | "TWh/y" => unit_kW(_value / 8.76E-6)
    else this
    end
  
  fun val to_MW() =>
    match _unit
    | "Nm/s" => unit_MW(_value / 1E6)
    | "J/s" => unit_MW(_value / 1E6)
    | "kJ/s" => unit_MW(_value / 1E3)
    | "MJ/s" => unit_MW(_value)
    | "GJ/s" => unit_MW(_value * 1E3)
    | "nW" => unit_MW(_value / 1E15)
    | "µW" => unit_MW(_value / 1E12)
    | "mW" => unit_MW(_value / 1E9)
    | "W" => unit_MW(_value / 1E6)
    | "kW" => unit_MW(_value / 1E3)
    | "MW" => this
    | "GW" => unit_MW(_value * 1E3)
    | "TW" => unit_MW(_value * 1E6)
    | "kWh/y" => unit_MW(_value / 8.76E6)
    | "MWh/y" => unit_MW(_value / 8.76E3)
    | "GWh/y" => unit_MW(_value / 8.76)
    | "TWh/y" => unit_MW(_value / 8.76E-3)
    else this
    end
  
  fun val to_GW() =>
    match _unit
    | "Nm/s" => unit_GW(_value / 1E9)
    | "J/s" => unit_GW(_value / 1E9)
    | "kJ/s" => unit_GW(_value / 1E6)
    | "MJ/s" => unit_GW(_value / 1E3)
    | "GJ/s" => unit_GW(_value)
    | "nW" => unit_GW(_value / 1E18)
    | "µW" => unit_GW(_value / 1E15)
    | "mW" => unit_GW(_value / 1E12)
    | "W" => unit_GW(_value / 1E9)
    | "kW" => unit_GW(_value / 1E6)
    | "MW" => unit_GW(_value / 1E3)
    | "GW" => this
    | "TW" => unit_GW(_value * 1E3)
    | "kWh/y" => unit_GW(_value / 8.76E9)
    | "MWh/y" => unit_GW(_value / 8.76E6)
    | "GWh/y" => unit_GW(_value / 8.76E3)
    | "TWh/y" => unit_GW(_value / 8.76)
    else this
    end
  
  fun val to_TW() =>
    match _unit
    | "Nm/s" => unit_TW(_value / 1E12)
    | "J/s" => unit_TW(_value / 1E12)
    | "kJ/s" => unit_TW(_value / 1E9)
    | "MJ/s" => unit_TW(_value / 1E6)
    | "GJ/s" => unit_TW(_value / 1E3)
    | "nW" => unit_TW(_value / 1E21)
    | "µW" => unit_TW(_value / 1E18)
    | "mW" => unit_TW(_value / 1E15)
    | "W" => unit_TW(_value / 1E12)
    | "kW" => unit_TW(_value / 1E9)
    | "MW" => unit_TW(_value / 1E6)
    | "GW" => unit_TW(_value / 1E6)
    | "TW" => this
    | "kWh/y" => unit_TW(_value / 8.76E12)
    | "MWh/y" => unit_TW(_value / 8.76E9)
    | "GWh/y" => unit_TW(_value / 8.76E6)
    | "TWh/y" => unit_TW(_value / 8.76E3)
    else this
    end
  
  fun val to_kWh_y() =>
    match _unit
    | "Nm/s" => unit_kWh_y(_value * 8.76)
    | "J/s" => unit_kWh_y(_value * 8.76)
    | "kJ/s" => unit_kWh_y(_value * 8.76E3)
    | "MJ/s" => unit_kWh_y(_value * 8.76E6 )
    | "GJ/s" => unit_kWh_y(_value * 8.76E9)
    | "nW" => unit_kWh_y(_value * 8.76E-9)
    | "µW" => unit_kWh_y(_value * 8.76E-6)
    | "mW" => unit_kWh_y(_value * 8.76E-3)
    | "W" => unit_kWh_y(_value * 8.76)
    | "kW" => unit_kWh_y(_value * 8.76E3)
    | "MW" => unit_kWh_y(_value * 8.76E6)
    | "GW" => unit_kWh_y(_value * 8.76E9)
    | "TW" => unit_kWh_y(_value * 8.76E12)
    | "kWh/y" => this
    | "MWh/y" => unit_kWh_y(_value * 1E3)
    | "GWh/y" => unit_kWh_y(_value * 1E6)
    | "TWh/y" => unit_kWh_y(_value * 1E9)
    else this
    end
    
  fun val to_MWh_y() =>
    match _unit
    | "Nm/s" => unit_MWh_y(_value * 8.76E-3)
    | "J/s" => unit_MWh_y(_value * 8.76E-3)
    | "kJ/s" => unit_MWh_y(_value * 8.76)
    | "MJ/s" => unit_MWh_y(_value * 8.76E3 )
    | "GJ/s" => unit_MWh_y(_value * 8.76E6)
    | "nW" => unit_MWh_y(_value * 8.76E-12)
    | "µW" => unit_MWh_y(_value * 8.76E-9)
    | "mW" => unit_MWh_y(_value * 8.76E-6)
    | "W" => unit_MWh_y(_value * 8.76E-3)
    | "kW" => unit_MWh_y(_value * 8.76)
    | "MW" => unit_MWh_y(_value * 8.76E3)
    | "GW" => unit_MWh_y(_value * 8.76E6)
    | "TW" => unit_MWh_y(_value * 8.76E9)
    | "kWh/y" => unit_MWh_y(_value * 1E-3)
    | "MWh/y" => this
    | "GWh/y" => unit_MWh_y(_value * 1E3)
    | "TWh/y" => unit_MWh_y(_value * 1E6)
    else this
    end
    

  fun val to_GWh_y() =>
    match _unit
    | "Nm/s" => unit_GWh_y(_value * 8.76E-6)
    | "J/s" => unit_GWh_y(_value * 8.76E-6)
    | "kJ/s" => unit_GWh_y(_value * 8.76E-3)
    | "MJ/s" => unit_GWh_y(_value * 8.76 )
    | "GJ/s" => unit_GWh_y(_value * 8.76E3)
    | "nW" => unit_GWh_y(_value * 8.76E-15)
    | "µW" => unit_GWh_y(_value * 8.76E-12)
    | "mW" => unit_GWh_y(_value * 8.76E-9)
    | "W" => unit_GWh_y(_value * 8.76E-6)
    | "kW" => unit_GWh_y(_value * 8.76E-3)
    | "MW" => unit_GWh_y(_value * 8.76)
    | "GW" => unit_GWh_y(_value * 8.76E3)
    | "TW" => unit_GWh_y(_value * 8.76E6)
    | "kWh/y" => unit_GWh_y(_value * 1E-6)
    | "MWh/y" => unit_GWh_y(_value * 1E-3)
    | "GWh/y" => this
    | "TWh/y" => unit_GWh_y(_value * 1E3)
    else this
    end
  
  fun val to_J_s() =>
    match _unit
    | "Nm/s" => unit_J_s(_value)
    | "J/s" => this
    | "kJ/s" => unit_J_s(_value * 1E3)
    | "MJ/s" => unit_J_s(_value * 1E6)
    | "GJ/s" => unit_J_s(_value * 1E9)
    | "nW" => unit_J_s(_value / 1E9)
    | "µW" => unit_J_s(_value / 1E6)
    | "mW" => unit_J_s(_value / 1E3)
    | "W" => unit_J_s(_value)
    | "kW" => unit_J_s(_value * 1E3)
    | "MW" => unit_J_s(_value * 1E6)
    | "GW" => unit_J_s(_value * 1E9)
    | "TW" => unit_J_s(_value * 1E12)
    | "kWh/y" => unit_J_s(_value / 8.76)
    | "MWh/y" => unit_J_s(_value / 8.76E-3)
    | "GWh/y" => unit_J_s(_value / 8.76E-6)
    | "TWh/y" => unit_J_s(_value / 8.76E-9)
    else this
    end
  
  fun val to_kJ_s() =>
    match _unit
    | "Nm/s" => unit_kJ_s(_value * 1E3)
    | "J/s" => unit_kJ_s(_value * 1E3)
    | "kJ/s" => this
    | "MJ/s" => unit_kJ_s(_value * 1E3)
    | "GJ/s" => unit_kJ_s(_value * 1E6)
    | "nW" => unit_kJ_s(_value / 1E12)
    | "µW" => unit_kJ_s(_value / 1E9)
    | "mW" => unit_kJ_s(_value / 1E6)
    | "W" => unit_kJ_s(_value / 1E3)
    | "kW" => unit_kJ_s(_value)
    | "MW" => unit_kJ_s(_value * 1E3)
    | "GW" => unit_kJ_s(_value * 1E6)
    | "TW" => unit_kJ_s(_value * 1E9)
    | "kWh/y" => unit_kJ_s(_value / 8.76E3)
    | "MWh/y" => unit_kJ_s(_value / 8.76)
    | "GWh/y" => unit_kJ_s(_value / 8.76E-3)
    | "TWh/y" => unit_kJ_s(_value / 8.76E-6)
    else this
    end
  
  fun val to_MJ_s() =>
    match _unit
    | "Nm/s" => unit_MJ_s(_value / 1E6)
    | "J/s" => unit_MJ_s(_value / 1E6)
    | "kJ/s" => unit_MJ_s(_value / 1E3)
    | "MJ/s" => this
    | "GJ/s" => unit_MJ_s(_value * 1E3)
    | "nW" => unit_MJ_s(_value / 1E15)
    | "µW" => unit_MJ_s(_value / 1E12)
    | "mW" => unit_MJ_s(_value / 1E9)
    | "W" => unit_MJ_s(_value / 1E6)
    | "kW" => unit_MJ_s(_value / 1E3)
    | "MW" => unit_MJ_s(_value)
    | "GW" => unit_MJ_s(_value * 1E3)
    | "TW" => unit_MJ_s(_value * 1E6)
    | "kWh/y" => unit_kJ_s(_value / 8.76E6)
    | "MWh/y" => unit_kJ_s(_value / 8.76E3)
    | "GWh/y" => unit_kJ_s(_value / 8.76)
    | "TWh/y" => unit_kJ_s(_value / 8.76E-3)
    else this
    end
  
  fun val to_GJ_s() =>
    match _unit
    | "Nm/s" => unit_GJ_s(_value / 1E9)
    | "J/s" => unit_GJ_s(_value / 1E9)
    | "kJ/s" => unit_GJ_s(_value / 1E6)
    | "MJ/s" => unit_GJ_s(_value / 1E3)
    | "GJ/s" => this
    | "nW" => unit_GJ_s(_value / 1E18)
    | "µW" => unit_GJ_s(_value / 1E15)
    | "mW" => unit_GJ_s(_value / 1E12)
    | "W" => unit_GJ_s(_value / 1E9)
    | "kW" => unit_GJ_s(_value / 1E6)
    | "MW" => unit_GJ_s(_value / 1E3)
    | "GW" => unit_GJ_s(_value)
    | "TW" => unit_GJ_s(_value * 1E3)
    | "kWh/y" => unit_kJ_s(_value / 8.76E9)
    | "MWh/y" => unit_kJ_s(_value / 8.76E6)
    | "GWh/y" => unit_kJ_s(_value / 8.76E3)
    | "TWh/y" => unit_kJ_s(_value / 8.76)
    else this
    end
  
  fun val to_Nm_s() =>
    match _unit
    | "Nm/s" => this
    | "J/s" => unit_W(_value)
    | "kJ/s" => unit_W(_value * 1E3)
    | "MJ/s" => unit_W(_value * 1E6)
    | "GJ/s" => unit_W(_value * 1E9)
    | "nW" => unit_W(_value / 1E9)
    | "µW" => unit_W(_value / 1E6)
    | "mW" => unit_W(_value / 1E3)
    | "W" => unit_W(_value)
    | "kW" => unit_W(_value * 1E3)
    | "MW" => unit_W(_value * 1E6)
    | "GW" => unit_W(_value * 1E9)
    | "TW" => unit_W(_value * 1E12)
    | "kWh/y" => unit_J_s(_value / 8.76)
    | "MWh/y" => unit_J_s(_value / 8.76E-3)
    | "GWh/y" => unit_J_s(_value / 8.76E-6)
    | "TWh/y" => unit_J_s(_value / 8.76E-9)
    else this
    end
  
