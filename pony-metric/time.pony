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

class val Time is Metric
  let _value:F64
  let _unit:String
  
  new val unit_s(value':F64) =>
    _value = value'
    _unit = "s"
    
  new val unit_m(value':F64) =>
    _value = value'
    _unit = "m"
    
  new val unit_h(value':F64) =>
    _value = value'
    _unit = "h"
    
  new val parse(text:String)? =>
    (_value, _unit) = MetricParser._extract(text)
    match _unit
    | "h" => None
    | "min" => None
    | "s" => None
    else error
    end
    
  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_value.string() + " " + _unit).string()

  fun val to_s():Time =>
    match _unit
    | "s" => this
    | "m" => unit_s(_value * 60)
    | "h" => unit_s(_value * 3600)
    else this
    end

  fun val to_m():Time =>
    match _unit
    | "s" => unit_m(_value / 60)
    | "m" => this
    | "h" => unit_m(_value * 3600)
    else this
    end
    
  fun val to_h():Time =>
    match _unit
    | "s" => unit_h(_value / 3600)
    | "m" => unit_h(_value / 60)
    | "h" => this
    else this
    end
