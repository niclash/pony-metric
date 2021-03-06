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

class val Temperature is Metric
  let _val:F64
  let _unit:String
  
  new val unit_C(value':F64) =>
    _val = value'
    _unit = "°C"

  new val unit_K(value':F64) =>
    _val = value'
    _unit = "K"

  new val unit_F(value':F64) =>
    _val = value'
    _unit = "°F"

  new val parse(text:String)? =>
    (_val, _unit) = MetricParser._extract(text)
    match _unit
    | "°C" => None
    | "K" => None
    | "°F" => None
    else error
    end
    
  fun val value():F64 =>
    _val
    
  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_val.string() + " " + _unit).string()

  fun val to_C():Temperature =>
    match _unit
    | "°C" => this
    | "°F" => unit_C((_val - 32) / 1.8)
    | "K" => unit_C(_val + 273.15)
    else this
    end

  fun val to_F():Temperature =>
    match _unit
    | "°C" => unit_F((_val * 1.8) + 32)
    | "°F" => this
    | "K" => unit_F((_val * 1.8) + 305.15)
    else this
    end

  fun val to_K():Temperature =>
    match _unit
    | "°C" => unit_K(_val - 273.15)
    | "°F" => unit_K(((_val - 32) / 1.8) - 273.15)
    | "K" => this
    else this
    end
