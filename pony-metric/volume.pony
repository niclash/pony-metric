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

class val Volume is Metric
  let _value:F64
  let _unit:String
  
  new val unit_m3(value':F64) =>
    _value = value'
    _unit = "m³"

  new val unit_dm3(value':F64) =>
    _value = value'
    _unit = "dm³"

  new val unit_cm3(value':F64) =>
    _value = value'
    _unit = "cm³"

  new val unit_mm3(value':F64) =>
    _value = value'
    _unit = "mm³"

  new val unit_km3(value':F64) =>
    _value = value'
    _unit = "km³"

  new val unit_l(value':F64) =>
    _value = value'
    _unit = "l"

  new val unit_dl(value':F64) =>
    _value = value'
    _unit = "dl"

  new val unit_cl(value':F64) =>
    _value = value'
    _unit = "cl"

  new val unit_ml(value':F64) =>
    _value = value'
    _unit = "ml"

  new val parse(text:String)? =>
    (_value, _unit) = MetricParser._extract(text)
    match _unit
    | "m³" => None
    | "dm³" => None
    | "cm³" => None
    | "mm³" => None
    | "km³" => None
    | "l" => None
    | "l" => None
    | "cl" => None
    | "ml" => None
    else error
    end
    
  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_value.string() + " " + _unit).string()

  fun val to_m3():Volume =>
    match _unit
    | "m³" => this
    | "dm³" => unit_m3(_value * 1E-3 )
    | "cm³" => unit_m3(_value * 1E-6 )
    | "mm³" => unit_m3(_value * 1E-9 )
    | "km³" => unit_m3(_value * 1E9 )
    | "l" => unit_m3(_value * 1E-3 )
    | "dl" => unit_m3(_value * 1E-4 )
    | "cl" => unit_m3(_value * 1E-5 )
    | "ml" => unit_m3(_value * 1E-6 )
    else this
    end

  fun val to_dm3():Volume =>
    match _unit
    | "m³" => unit_dm3(_value * 1E3 )
    | "dm³" => this
    | "cm³" => unit_dm3(_value * 1E-3 )
    | "mm³" => unit_dm3(_value * 1E-6 )
    | "km³" => unit_dm3(_value * 1E12 )
    | "l" => unit_dm3(_value )
    | "dl" => unit_dm3(_value * 1E-1 )
    | "cl" => unit_dm3(_value * 1E-2 )
    | "ml" => unit_dm3(_value * 1E-3 )
    else this
    end

  fun val to_cm3():Volume =>
    match _unit
    | "m³" => unit_cm3(_value * 1E6 )
    | "dm³" => unit_cm3(_value * 1E3 )
    | "cm³" => this
    | "mm³" => unit_cm3(_value * 1E-3 )
    | "km³" => unit_cm3(_value * 1E15 )
    | "l" => unit_cm3(_value * 1E3 )
    | "dl" => unit_cm3(_value * 1E2 )
    | "cl" => unit_cm3(_value * 1E1 )
    | "ml" => unit_cm3(_value )
    else this
    end

  fun val to_mm3():Volume =>
    match _unit
    | "m³" => unit_mm3(_value * 1E9 )
    | "dm³" => unit_mm3(_value * 1E6 )
    | "cm³" => unit_mm3(_value * 1E3 )
    | "mm³" => this
    | "km³" => unit_mm3(_value * 1E18 )
    | "l" => unit_mm3(_value * 1E6 )
    | "dl" => unit_mm3(_value * 1E5 )
    | "cl" => unit_mm3(_value * 1E4 )
    | "ml" => unit_mm3(_value * 1E3 )
    else this
    end

  fun val to_km3():Volume =>
    match _unit
    | "m³" => unit_km3(_value * 1E-9 )
    | "dm³" => unit_km3(_value * 1E-12 )
    | "cm³" => unit_km3(_value * 1E-15 )
    | "mm³" => unit_km3(_value * 1E-18 )
    | "km³" => this
    | "l" => unit_km3(_value * 1E-12 )
    | "dl" => unit_km3(_value * 1E-13 )
    | "cl" => unit_km3(_value * 1E-14 )
    | "ml" => unit_km3(_value * 1E-15 )
    else this
    end

  fun val to_l():Volume =>
    match _unit
    | "m³" => unit_l(_value * 1E3 )
    | "dm³" => unit_l(_value )
    | "cm³" => unit_l(_value * 1E-3 )
    | "mm³" => unit_l(_value * 1E-6 )
    | "km³" => unit_l(_value * 1E12 )
    | "l" => this
    | "dl" => unit_l(_value * 1E-1 )
    | "cl" => unit_l(_value * 1E-2 )
    | "ml" => unit_l(_value * 1E-3 )
    else this
    end

  fun val to_dl():Volume =>
    match _unit
    | "m³" => unit_dl(_value * 1E4 )
    | "dm³" => unit_dl(_value * 1E1 )
    | "cm³" => unit_dl(_value * 1E-2 )
    | "mm³" => unit_dl(_value * 1E-5 )
    | "km³" => unit_dl(_value * 1E13 )
    | "l" => unit_dl(_value * 1E1)
    | "dl" => this
    | "cl" => unit_dl(_value * 1E-1 )
    | "ml" => unit_dl(_value * 1E-2 )
    else this
    end

  fun val to_cl():Volume =>
    match _unit
    | "m³" => unit_l(_value * 1E5 )
    | "dm³" => unit_l(_value * 1E2 )
    | "cm³" => unit_l(_value * 1E-1 )
    | "mm³" => unit_l(_value * 1E-4 )
    | "km³" => unit_l(_value * 1E14 )
    | "l" => unit_l(_value * 1E2)
    | "dl" => unit_l(_value * 1E1 )
    | "cl" => this
    | "ml" => unit_l(_value * 1E-1 )
    else this
    end

  fun val to_ml():Volume =>
    match _unit
    | "m³" => unit_ml(_value * 1E6 )
    | "dm³" => unit_ml(_value * 1E3 )
    | "cm³" => unit_ml(_value )
    | "mm³" => unit_ml(_value * 1E-3 )
    | "km³" => unit_ml(_value * 1E15 )
    | "l" => unit_ml(_value * 1E3)
    | "dl" => unit_ml(_value * 1E2 )
    | "cl" => unit_ml(_value * 1E1 )
    | "ml" => this
    else this
    end
