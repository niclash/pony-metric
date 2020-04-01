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

class val Current is Metric
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

  new val parse(text:String)? =>
    (_value, _unit) = MetricParser._extract(text)
    match _unit
    | "A" => None
    | "kA" => None
    | "mA" => None
    | "µA" => None
    | "nA²" => None
    | "pA²" => None
    else error
    end
    
  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_value.string() + " " + _unit).string()

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

