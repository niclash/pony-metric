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

class val Area is Metric
  let _val:F64
  let _unit:String
  
  new val unit_m2(value':F64) =>
    _val = value'
    _unit = "m²"

  new val unit_dm2(value':F64) =>
    _val = value'
    _unit = "dm²"

  new val unit_cm2(value':F64) =>
    _val = value'
    _unit = "cm²"

  new val unit_mm2(value':F64) =>
    _val = value'
    _unit = "mm²"

  new val unit_km2(value':F64) =>
    _val = value'
    _unit = "km²"

  new val parse(text:String)? =>
    (_val, _unit) = MetricParser._extract(text)
    match _unit
    | "m²" => None
    | "dm²" => None
    | "cm²" => None
    | "mm²" => None
    | "km²" => None
    else error
    end
    
  fun val value():F64 =>
    _val
    
  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_val.string() + " " + _unit).string()

  fun val to_m2():Area =>
    match _unit
    | "m²" => this
    | "dm²" => unit_m2(_val * 1E-2 )
    | "cm²" => unit_m2(_val * 1E-4 )
    | "mm²" => unit_m2(_val * 1E-6 )
    | "km²" => unit_m2(_val * 1E6 )
    else this
    end

  fun val to_dm2():Area =>
    match _unit
    | "m²" => unit_dm2(_val * 1E2 )
    | "dm²" => this
    | "cm²" => unit_dm2(_val * 1E-2 )
    | "mm²" => unit_dm2(_val * 1E-4 )
    | "km²" => unit_dm2(_val * 1E8 )
    else this
    end

  fun val to_cm2():Area =>
    match _unit
    | "m²" => unit_cm2(_val * 1E4 )
    | "dm²" => unit_cm2(_val * 1E2 )
    | "cm²" => this
    | "mm²" => unit_cm2(_val * 1E-2 )
    | "km²" => unit_cm2(_val * 1E10 )
    else this
    end

  fun val to_mm2():Area =>
    match _unit
    | "m²" => unit_mm2(_val * 1E6 )
    | "dm²" => unit_mm2(_val * 1E4 )
    | "cm²" => unit_mm2(_val * 1E2 )
    | "mm²" => this
    | "km²" => unit_mm2(_val * 1E12 )
    else this
    end

  fun val to_km2():Area =>
    match _unit
    | "m²" => unit_km2(_val * 1E-6 )
    | "dm²" => unit_km2(_val * 1E-8 )
    | "cm²" => unit_km2(_val * 1E-10 )
    | "mm²" => unit_km2(_val * 1E-12 )
    | "km²" => this
    else this
    end
