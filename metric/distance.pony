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

class val Distance is Metric
  let _val:F64
  let _unit:String
  
  new val unit_m(value':F64) =>
    _val = value'
    _unit = "m"

  new val unit_dm(value':F64) =>
    _val = value'
    _unit = "dm"

  new val unit_cm(value':F64) =>
    _val = value'
    _unit = "cm"

  new val unit_mm(value':F64) =>
    _val = value'
    _unit = "mm"

  new val unit_km(value':F64) =>
    _val = value'
    _unit = "km"

  new val parse(text:String)? =>
    (_val, _unit) = MetricParser._extract(text)
    match _unit
    | "m" => None
    | "dm" => None
    | "cm" => None
    | "mm" => None
    | "km" => None
    else error
    end
    
  fun val value():F64 =>
    _val
    
  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_val.string() + " " + _unit).string()

  fun val to_m():Distance =>
    match _unit
    | "m" => this
    | "dm" => unit_m(_val * 1E-1 )
    | "cm" => unit_m(_val * 1E-2 )
    | "mm" => unit_m(_val * 1E-3 )
    | "km" => unit_m(_val * 1E3 )
    else this
    end

  fun val to_dm():Distance =>
    match _unit
    | "m" => unit_dm(_val * 1E1 )
    | "dm" => this
    | "cm" => unit_dm(_val * 1E-1 )
    | "mm" => unit_dm(_val * 1E-2 )
    | "km" => unit_dm(_val * 1E4 )
    else this
    end

  fun val to_cm():Distance =>
    match _unit
    | "m" => unit_cm(_val * 1E2 )
    | "dm" => unit_cm(_val * 1E1 )
    | "cm" => this
    | "mm" => unit_cm(_val * 1E-1 )
    | "km" => unit_cm(_val * 1E5 )
    else this
    end

  fun val to_mm():Distance =>
    match _unit
    | "m" => unit_mm(_val * 1E3 )
    | "dm" => unit_mm(_val * 1E2 )
    | "cm" => unit_mm(_val * 1E1 )
    | "mm" => this
    | "km" => unit_mm(_val * 1E6 )
    else this
    end

  fun val to_km():Distance =>
    match _unit
    | "m" => unit_km(_val * 1E-3 )
    | "dm" => unit_km(_val * 1E-4 )
    | "cm" => unit_km(_val * 1E-5 )
    | "mm" => unit_km(_val * 1E-6 )
    | "km" => this
    else this
    end
