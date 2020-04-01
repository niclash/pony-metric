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

class val Mass is Metric
  let _value:F64
  let _unit:String
  
  new val unit_ug(value':F64) =>
    _value = value'
    _unit = "µg"

  new val unit_mg(value':F64) =>
    _value = value'
    _unit = "mg"

  new val unit_g(value':F64) =>
    _value = value'
    _unit = "g"

  new val unit_kg(value':F64) =>
    _value = value'
    _unit = "kg"

  new val parse(text:String)? =>
    (_value, _unit) = MetricParser._extract(text)
    match _unit
    | "µg" => None
    | "mg" => None
    | "g" => None
    | "kg" => None
    else error
    end
    
  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_value.string() + " " + _unit).string()

  fun val to_kg():Mass =>
    match _unit
    | "µg" => unit_kg(_value / 1000000000)
    | "mg" => unit_kg(_value / 1000000)
    | "g" => unit_kg(_value / 1000)
    | "kg" => this
    else this
    end

  fun val to_g():Mass =>
    match _unit
    | "µg" => unit_g(_value / 1000000)
    | "mg" => unit_g(_value / 1000)
    | "g" => this
    | "kg" => unit_g(_value * 1000)
    else this
    end

  fun val to_mg():Mass =>
    match _unit
    | "µg" => unit_mg(_value / 1000)
    | "mg" => this
    | "g" => unit_mg(_value * 1000)
    | "kg" => unit_mg(_value * 1000000)
    else this
    end

  fun val to_ug():Mass =>
    match _unit
    | "µg" => this
    | "mg" => unit_ug(_value * 1000)
    | "g" => unit_ug(_value * 1000000)
    | "kg" => unit_ug(_value * 1000000000)
    else this
    end
