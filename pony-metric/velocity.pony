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

class val Velocity is Metric
  let _val:F64
  let _unit:String
  
  new val unit_m_s(value':F64) =>
    _val = value'
    _unit = "m/s"
    
  new val unit_km_h(value':F64) =>
    _val = value'
    _unit = "km/h"
    
  new val parse(text:String)? =>
    (_val, _unit) = MetricParser._extract(text)
    match _unit
    | "m/s" => None
    | "km/h" => None
    else error
    end
    
  fun val value():F64 =>
    _val
    
  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_val.string() + " " + _unit).string()

  fun val to_m_s():Velocity =>
    match _unit
    | "m/s" => this
    | "km/h" => unit_m_s(_val / 3.6)
    else this
    end
    
  fun val to_km_h():Velocity =>
    match _unit
    | "m/s" => unit_km_h(_val * 3.6)
    | "km/h" => this
    else this
    end
