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

class val Energy is Metric
  let _value:F64
  let _unit:String
  
  new val unit_Nm(value':F64) =>
    _value=value'
    _unit = "Nm"
    
  new val unit_Ws(value':F64) =>
    _value=value'
    _unit = "Ws"
    
  new val unit_Wh(value':F64) =>
    _value=value'
    _unit = "Wh"
    
  new val unit_kWh(value':F64) =>
    _value=value'
    _unit = "kWh"
    
  new val unit_MWh(value':F64) =>
    _value=value'
    _unit = "MWh"
    
  new val unit_GWh(value':F64) =>
    _value=value'
    _unit = "GWh"
    
  new val unit_TWh(value':F64) =>
    _value=value'
    _unit = "TWh"
    
  new val unit_J(value':F64) =>
    _value=value'
    _unit = "J"
    
  new val unit_kJ(value':F64) =>
    _value=value'
    _unit = "kJ"
    
  new val unit_MJ(value':F64) =>
    _value=value'
    _unit = "MJ"
    
  new val unit_GJ(value':F64) =>
    _value=value'
    _unit = "GJ"
    
  new val unit_TJ(value':F64) =>
    _value=value'
    _unit = "TJ"
    
  new val parse(text:String)? =>
    (_value, _unit) = MetricParser._extract(text)
    match _unit
    | "Nm" => None
    | "Ws" => None
    | "Wh" => None
    | "kWh" => None
    | "MWh" => None
    | "GWh" => None
    | "TWh" => None
    | "J" => None
    | "kJ" => None
    | "MJ" => None
    | "GJ" => None
    | "TJ" => None
    else error
    end
  
  fun val value():F64 =>
    _value

  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_value.string() + " " + _unit).string()

  fun val to_Power(time:Periodicity):Power =>
    Power.unit_W(to_J()._value / time.to_s().value())
    
  fun val div(value':F64):Energy =>
    match _unit
    | "Nm" => unit_Nm(_value / value')
    | "Ws" => unit_Ws(_value / value')
    | "Wh" => unit_Wh(_value / value')
    | "kWh" => unit_kWh(_value / value')
    | "MWh" => unit_MWh(_value / value')
    | "GWh" => unit_GWh(_value / value')
    | "TWh" => unit_TWh(_value / value')
    | "J" => unit_J(_value / value')
    | "kJ" => unit_kJ(_value / value')
    | "MJ" => unit_MJ(_value / value')
    | "GJ" => unit_GJ(_value / value')
    | "TJ" => unit_TJ(_value / value')
    else this
    end

  fun val mul(value':F64):Energy =>
    match _unit
    | "Nm" => unit_Nm(_value * value')
    | "Ws" => unit_Ws(_value * value')
    | "Wh" => unit_Wh(_value * value')
    | "kWh" => unit_kWh(_value * value')
    | "MWh" => unit_MWh(_value * value')
    | "GWh" => unit_GWh(_value * value')
    | "TWh" => unit_TWh(_value * value')
    | "J" => unit_J(_value * value')
    | "kJ" => unit_kJ(_value * value')
    | "MJ" => unit_MJ(_value * value')
    | "GJ" => unit_GJ(_value * value')
    | "TJ" => unit_TJ(_value * value')
    else this
    end

  fun val to_Nm():Energy =>
    match _unit
    | "Nm" => this
    | "Ws" => unit_Nm(_value)
    | "Wh" => unit_Nm(_value * 3600)
    | "kWh" => unit_Nm(_value * 3600000)
    | "MWh" => unit_Nm(_value * 3600000000)
    | "GWh" => unit_Nm(_value * 3600000000000)
    | "TWh" => unit_Nm(_value * 3600000000000000)
    | "J" => unit_Nm(_value)
    | "kJ" => unit_Nm(_value * 1000)
    | "MJ" => unit_Nm(_value * 1000000)
    | "GJ" => unit_Nm(_value * 1000000000)
    | "TJ" => unit_Nm(_value * 1000000000000)
    else this
    end
    
  fun val to_Ws():Energy =>
    match _unit
    | "Nm" => unit_Ws(_value)
    | "Ws" => this
    | "Wh" => unit_Ws(_value * 3600)
    | "kWh" => unit_Ws(_value * 3600000)
    | "MWh" => unit_Ws(_value * 3600000000)
    | "GWh" => unit_Ws(_value * 3600000000000)
    | "TWh" => unit_Ws(_value * 3600000000000000)
    | "J" => unit_Ws(_value)
    | "kJ" => unit_Ws(_value * 1000)
    | "MJ" => unit_Ws(_value * 1000000)
    | "GJ" => unit_Ws(_value * 1000000000)
    | "TJ" => unit_Ws(_value * 1000000000000)
    else this
    end
    
  fun val to_Wh():Energy =>
    match _unit
    | "Nm" => unit_Wh(_value/3600)
    | "Ws" => unit_Wh(_value/3600)
    | "Wh" => this
    | "kWh" => unit_Wh(_value * 1000)
    | "MWh" => unit_Wh(_value * 1000000)
    | "GWh" => unit_Wh(_value * 1000000000)
    | "TWh" => unit_Wh(_value * 1000000000000)
    | "J" => unit_Wh(_value/3600)
    | "kJ" => unit_Wh(_value / 3.6)
    | "MJ" => unit_Wh(_value / 0.0036)
    | "GJ" => unit_Wh(_value / 0.0000036)
    | "TJ" => unit_Wh(_value / 0.0000000036)
    else this
    end
    
    
  fun val to_kWh():Energy =>
    match _unit
    | "Nm" => unit_kWh(_value/3600000)
    | "Ws" => unit_kWh(_value/3600000)
    | "Wh" => unit_kWh(_value/1000)
    | "kWh" => this
    | "MWh" => unit_kWh(_value * 1000)
    | "GWh" => unit_kWh(_value * 1000000)
    | "TWh" => unit_kWh(_value * 1000000000)
    | "J" => unit_kWh(_value/3600000)
    | "kJ" => unit_kWh(_value / 36000)
    | "MJ" => unit_kWh(_value / 3.6)
    | "GJ" => unit_kWh(_value / 0.0036)
    | "TJ" => unit_kWh(_value / 0.0000036)
    else this
    end
    
  fun val to_MWh():Energy =>
    match _unit
    | "Nm" => unit_MWh(_value/3600000000)
    | "Ws" => unit_MWh(_value/3600000000)
    | "Wh" => unit_MWh(_value/1000000)
    | "kWh" => unit_MWh(_value / 1000 )
    | "MWh" => this
    | "GWh" => unit_MWh(_value * 1000)
    | "TWh" => unit_MWh(_value * 1000000)
    | "J" => unit_MWh(_value/3600000000)
    | "kJ" => unit_MWh(_value / 36000000)
    | "MJ" => unit_MWh(_value / 36000)
    | "GJ" => unit_MWh(_value / 3.6)
    | "TJ" => unit_MWh(_value / 0.0036)
    else this
    end
    
  fun val to_GWh():Energy =>
    match _unit
    | "Nm" => unit_GWh(_value/3600000000000)
    | "Ws" => unit_GWh(_value/3600000000000)
    | "Wh" => unit_GWh(_value/1000000000)
    | "kWh" => unit_GWh(_value / 1000000 )
    | "MWh" => unit_GWh(_value / 1000 )
    | "GWh" => this
    | "TWh" => unit_GWh(_value * 1000)
    | "J" => unit_GWh(_value/3600000000000)
    | "kJ" => unit_GWh(_value / 36000000000)
    | "MJ" => unit_GWh(_value / 36000000)
    | "GJ" => unit_GWh(_value / 3600)
    | "TJ" => unit_GWh(_value / 3.6)
    else this
    end
    
  fun val to_TWh():Energy =>
    match _unit
    | "Nm" => unit_TWh(_value/3600000000000000)
    | "Ws" => unit_TWh(_value/3600000000000000)
    | "Wh" => unit_TWh(_value/1000000000000)
    | "kWh" => unit_TWh(_value / 1000000000 )
    | "MWh" => unit_TWh(_value / 1000000 )
    | "GWh" => unit_TWh(_value / 1000)
    | "TWh" => this
    | "J" => unit_TWh(_value/3600000000000000)
    | "kJ" => unit_TWh(_value / 36000000000000)
    | "MJ" => unit_TWh(_value / 36000000000)
    | "GJ" => unit_TWh(_value / 3600000)
    | "TJ" => unit_TWh(_value / 3600)
    else this
    end
    
  fun val to_J():Energy =>
    match _unit
    | "Nm" => unit_J(_value)
    | "Ws" => unit_J(_value)
    | "Wh" => unit_J(_value * 3600)
    | "kWh" => unit_J(_value * 3600000)
    | "MWh" => unit_J(_value * 3600000000)
    | "GWh" => unit_J(_value * 3600000000000)
    | "TWh" => unit_J(_value * 3600000000000000)
    | "J" => this
    | "kJ" => unit_J(_value * 1000)
    | "MJ" => unit_J(_value * 1000000)
    | "GJ" => unit_J(_value * 1000000000)
    | "TJ" => unit_J(_value * 1000000000000)
    else this
    end
    
  fun val to_kJ():Energy =>
    match _unit
    | "Nm" => unit_kJ(_value/1000)
    | "Ws" => unit_kJ(_value/1000)
    | "Wh" => unit_kJ(_value * 3.600)
    | "kWh" => unit_kJ(_value * 3600)
    | "MWh" => unit_kJ(_value * 3600000)
    | "GWh" => unit_kJ(_value * 3600000000)
    | "TWh" => unit_kJ(_value * 3600000000000)
    | "J" => unit_kJ(_value/1000)
    | "kJ" => this
    | "MJ" => unit_kJ(_value * 1000)
    | "GJ" => unit_kJ(_value * 1000000)
    | "TJ" => unit_kJ(_value * 1000000000)
    else this
    end
    
  fun val to_MJ():Energy =>
    match _unit
    | "Nm" => unit_MJ(_value/1000000)
    | "Ws" => unit_MJ(_value/1000000)
    | "Wh" => unit_MJ(_value * 0.0036)
    | "kWh" => unit_MJ(_value * 3.6)
    | "MWh" => unit_MJ(_value * 3600)
    | "GWh" => unit_MJ(_value * 3600000)
    | "TWh" => unit_MJ(_value * 3600000000)
    | "J" => unit_MJ(_value/1000000)
    | "kJ" => unit_MJ(_value / 1000)
    | "MJ" => this
    | "GJ" => unit_MJ(_value * 1000)
    | "TJ" => unit_MJ(_value * 1000000)
    else this
    end
    
  fun val to_GJ():Energy =>
    match _unit
    | "Nm" => unit_GJ(_value/1000000000)
    | "Ws" => unit_GJ(_value/1000000000)
    | "Wh" => unit_GJ(_value * 0.0000036)
    | "kWh" => unit_GJ(_value * 0.0036)
    | "MWh" => unit_GJ(_value * 3.6)
    | "GWh" => unit_GJ(_value * 3600)
    | "TWh" => unit_GJ(_value * 3600000)
    | "J" => unit_GJ(_value/1000000000)
    | "kJ" => unit_GJ(_value / 1000000)
    | "MJ" => unit_GJ(_value / 1000)
    | "GJ" => this
    | "TJ" => unit_GJ(_value * 1000)
    else this
    end
    
  fun val to_TJ():Energy =>
    match _unit
    | "Nm" => unit_TJ(_value/1000000000000)
    | "Ws" => unit_TJ(_value/1000000000000)
    | "Wh" => unit_TJ(_value * 0.0000000036)
    | "kWh" => unit_TJ(_value * 0.0000036)
    | "MWh" => unit_TJ(_value * 0.0036)
    | "GWh" => unit_TJ(_value * 3.6)
    | "TWh" => unit_TJ(_value * 3600)
    | "J" => unit_TJ(_value/1000000000000)
    | "kJ" => unit_TJ(_value / 1000000000)
    | "MJ" => unit_TJ(_value / 1000000)
    | "GJ" => unit_TJ(_value / 1000)
    | "TJ" => this
    else this
    end

  fun val add(value':Energy):Energy =>
    match _unit
    | "Nm" => unit_Nm(_value + value'.to_Nm()._value)
    | "Ws" => unit_Ws(_value + value'.to_Ws()._value)
    | "Wh" => unit_Wh(_value + value'.to_Wh()._value)
    | "kWh" => unit_kWh(_value + value'.to_kWh()._value)
    | "MWh" => unit_MWh(_value + value'.to_MWh()._value)
    | "GWh" => unit_GWh(_value + value'.to_GWh()._value)
    | "TWh" => unit_TWh(_value + value'.to_TWh()._value)
    | "J" => unit_J(_value + value'.to_J()._value)
    | "kJ" => unit_kJ(_value + value'.to_kJ()._value)
    | "MJ" => unit_MJ(_value + value'.to_MJ()._value)
    | "GJ" => unit_GJ(_value + value'.to_GJ()._value)
    | "TJ" => unit_TJ(_value + value'.to_TJ()._value)
    else this
    end

  fun val sub(value':Energy):Energy =>
    match _unit
    | "Nm" => unit_Nm(_value - value'.to_Nm()._value)
    | "Ws" => unit_Ws(_value - value'.to_Ws()._value)
    | "Wh" => unit_Wh(_value - value'.to_Wh()._value)
    | "kWh" => unit_kWh(_value - value'.to_kWh()._value)
    | "MWh" => unit_MWh(_value - value'.to_MWh()._value)
    | "GWh" => unit_GWh(_value - value'.to_GWh()._value)
    | "TWh" => unit_TWh(_value - value'.to_TWh()._value)
    | "J" => unit_J(_value - value'.to_J()._value)
    | "kJ" => unit_kJ(_value - value'.to_kJ()._value)
    | "MJ" => unit_MJ(_value - value'.to_MJ()._value)
    | "GJ" => unit_GJ(_value - value'.to_GJ()._value)
    | "TJ" => unit_TJ(_value - value'.to_TJ()._value)
    else this
    end

