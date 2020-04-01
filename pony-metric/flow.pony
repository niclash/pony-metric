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

class val Flow is Metric
  let _value:F64
  let _unit:String
  
  new val unit_m3_h(value':F64) =>
    _value = value'
    _unit = "m³/h"

  new val unit_m3_s(value':F64) =>
    _value = value'
    _unit = "m³/s"

  new val unit_l_s(value':F64) =>
    _value = value'
    _unit = "l/s"

  new val parse(text:String)? =>
    (_value, _unit) = MetricParser._extract(text)
    match _unit
    | "m³/h" => None
    | "m³/s" => None
    | "l/s" => None
    else error
    end

  fun val value():F64 =>
    _value
    
  fun val unit():String =>
    _unit
    
  fun box string(): String iso^ =>
    (_value.string() + " " + _unit).string()

  fun val to_m3_h():Flow =>
    match _unit
    | "m³/h" => this
    | "m³/s" => unit_m3_h(_value * 3600)
    | "l/s" => unit_m3_h(_value * 3.6)
    else this
    end

  fun val to_m3_s():Flow =>
    match _unit
    | "m³/h" => unit_m3_s(_value * 3600)
    | "m³/s" => this
    | "l/s" => unit_m3_s(_value * 1000)
    else this
    end

  fun val to_l_s():Flow =>
    match _unit
    | "m³/h" => unit_l_s(_value / 3.6)
    | "m³/s" => unit_l_s(_value / 1000)
    | "l/s" => this
    else this
    end
