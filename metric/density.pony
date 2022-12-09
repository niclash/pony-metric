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

class val Density is Metric
  let _val:F64
  let _unit:String
  
  new val unit_kg_m3(value':F64) =>
    _val = value'
    _unit = "kg/m³"

  new val unit_kg_dm3(value':F64) =>
    _val = value'
    _unit = "kg/dm³"

  new val unit_g_cm3(value':F64) =>
    _val = value'
    _unit = "g/cm³"

  new val parse(text:String)? =>
    (_val, _unit) = MetricParser._extract(text)
    match _unit
    | "kg/m³" => None
    | "kg/dm³" => None
    | "g/cm³" => None
    else error
    end
    
  fun val value():F64 =>
    _val
    
  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_val.string() + " " + _unit).string()

  fun val to_kg_m3():Density =>
    match _unit
    | "kg/m³" => this
    | "kg/dm³" => unit_kg_m3(_val * 1000 )
    | "g/cm³" => unit_kg_m3(_val * 1000)
    else this
    end

  fun val to_kg_dm3():Density =>
    match _unit
    | "kg/m³" => unit_kg_dm3(_val / 1000 )
    | "kg/dm³" => this
    | "g/cm³" => unit_kg_dm3(_val)
    else this
    end

  fun val to_g_cm3():Density =>
    match _unit
    | "kg/m³" => unit_g_cm3(_val / 1000)
    | "kg/dm³" => unit_g_cm3(_val)
    | "g/cm³" => this
    else this
    end
