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

class val Frequency is Metric
  let _value:F64
  let _unit:String
  
  new val unit_Hz(value':F64) =>
    _value = value'
    _unit = "Hz"

  new val unit_kHz(value':F64) =>
    _value = value'
    _unit = "kHz"

  new val unit_MHz(value':F64) =>
    _value = value'
    _unit = "MHz"

  new val unit_GHz(value':F64) =>
    _value = value'
    _unit = "GHz"

  new val parse(text:String)? =>
    (_value, _unit) = MetricParser._extract(text)
    match _unit
    | "Hz" => None
    | "kHz" => None
    | "MHz" => None
    | "GHz" => None
    else error
    end

  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_value.string() + " " + _unit).string()

  fun val to_Hz():Frequency =>
    match _unit
    | "Hz" => this
    | "kHz" => unit_Hz(_value * 1000)
    | "MHz" => unit_Hz(_value * 1000000)
    | "GHz" => unit_Hz(_value * 1000000000)
    else this
    end

  fun val to_kHz():Frequency =>
    match _unit
    | "Hz" => unit_kHz(_value / 1000 )
    | "kHz" => this
    | "MHz" => unit_kHz(_value * 1000 )
    | "GHz" => unit_kHz(_value * 1000000 )
    else this
    end

  fun val to_MHz():Frequency =>
    match _unit
    | "Hz" => unit_MHz(_value / 1000000 )
    | "kHz" => unit_MHz(_value / 1000 )
    | "MHz" => this
    | "GHz" => unit_MHz(_value * 1000 )
    else this
    end

  fun val to_GHz():Frequency =>
    match _unit
    | "Hz" => unit_GHz(_value / 1000000000 )
    | "kHz" => unit_GHz(_value / 1000000 )
    | "MHz" => unit_GHz(_value / 1000 )
    | "GHz" => this
    else this
    end

    
