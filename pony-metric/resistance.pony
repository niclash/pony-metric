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

class val Resistance is Metric
  let _val:F64
  let _unit:String
  
  new val unit_ohm(value':F64) =>
    _val = value'
    _unit = "Ω"

  new val unit_kilo_ohm(value':F64) =>
    _val = value'
    _unit = "kΩ"

  new val unit_mega_ohm(value':F64) =>
    _val = value'
    _unit = "MΩ"

  new val unit_giga_ohm(value':F64) =>
    _val = value'
    _unit = "GΩ"

  new val unit_milli_ohm(value':F64) =>
    _val = value'
    _unit = "mΩ"

  new val parse(text:String)? =>
    (_val, _unit) = MetricParser._extract(text)
    match _unit
    | "Ω" => None
    | "kΩ" => None
    | "MΩ" => None
    | "GΩ" => None
    | "mΩ" => None
    else error
    end
    
  fun val value():F64 =>
    _val
    
  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_val.string() + " " + _unit).string()

  fun val to_milli_ohm():Resistance =>
    match _unit
    | "mΩ" => this
    | "Ω" => unit_milli_ohm(_val * 1000)
    | "kΩ" => unit_milli_ohm(_val * 1000000)
    | "MΩ" => unit_milli_ohm(_val * 1000000000)
    | "GΩ" => unit_milli_ohm(_val * 1000000000000)
    else this
    end

  fun val to_ohm():Resistance =>
    match _unit
    | "mΩ" => unit_ohm(_val / 1000)
    | "Ω" => this
    | "kΩ" => unit_ohm(_val * 1000)
    | "MΩ" => unit_ohm(_val * 1000000)
    | "GΩ" => unit_ohm(_val * 1000000000)
    else this
    end


  fun val to_kilo_ohm():Resistance =>
    match _unit
    | "mΩ" => unit_kilo_ohm(_val / 1000000)
    | "Ω" => unit_kilo_ohm(_val / 1000)
    | "kΩ" => this
    | "MΩ" => unit_kilo_ohm(_val * 1000)
    | "GΩ" => unit_kilo_ohm(_val * 1000000)
    else this
    end

  fun val to_mega_ohm():Resistance =>
    match _unit
    | "mΩ" => unit_mega_ohm(_val / 1000000000)
    | "Ω" => unit_mega_ohm(_val / 1000000)
    | "kΩ" => unit_mega_ohm(_val / 1000)
    | "MΩ" => this
    | "GΩ" => unit_mega_ohm(_val * 1000)
    else this
    end

  fun val to_giga_ohm():Resistance =>
    match _unit
    | "mΩ" => unit_giga_ohm(_val / 1000000000000)
    | "Ω" => unit_giga_ohm(_val / 1000000000)
    | "kΩ" => unit_giga_ohm(_val / 1000000)
    | "MΩ" => unit_giga_ohm(_val / 1000)
    | "GΩ" => this
    else this
    end
    
