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

class val Pressure is Metric
  let _value:F64
  let _unit:String
  
  new val unit_Pa(value':F64) =>
    _value = value'
    _unit = "Pa"

  new val unit_milli_Pa(value':F64) =>
    _value = value'
    _unit = "mPa"

  new val unit_kPa(value':F64) =>
    _value = value'
    _unit = "kPa"

  new val unit_MPa(value':F64) =>
    _value = value'
    _unit = "MPa"

  new val parse(text:String)? =>
    (_value, _unit) = MetricParser._extract(text)
    match _unit
    | "Pa" => None
    | "mPa" => None
    | "kPa" => None
    | "MPa" => None
    else error
    end
    
  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_value.string() + " " + _unit).string()

  fun val to_Pa():Pressure =>
    match _unit
    | "Pa" => this
    | "mPa" => unit_Pa(_value / 1000)
    | "kPa" => unit_Pa(_value * 1000)
    | "MPa" => unit_Pa(_value * 1000000)
    else this
    end

  fun val to_milli_Pa():Pressure =>
    match _unit
    | "Pa" => unit_milli_Pa(_value * 1000)
    | "mPa" => this
    | "kPa" => unit_milli_Pa(_value * 1000000)
    | "MPa" => unit_milli_Pa(_value * 1000000000)
    else this
    end

  fun val to_kPa():Pressure =>
    match _unit
    | "Pa" => unit_kPa(_value / 1000)
    | "mPa" => unit_kPa(_value / 1000000)
    | "kPa" => this
    | "MPa" => unit_kPa(_value * 1000)
    else this
    end

  fun val to_MPa():Pressure =>
    match _unit
    | "Pa" => unit_kPa(_value / 1000000)
    | "mPa" => unit_MPa(_value / 1000000000)
    | "kPa" => unit_MPa(_value / 1000)
    | "MPa" => this
    else this
    end

