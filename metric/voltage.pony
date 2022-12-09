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

class val Voltage is Metric
  let _val:F64
  let _unit:String
  
  new val unit_V(value':F64) =>
    _val = value'
    _unit = "V"

  new val unit_kV(value':F64) =>
    _val = value'
    _unit = "kV"

  new val unit_mV(value':F64) =>
    _val = value'
    _unit = "mV"

  new val unit_uV(value':F64) =>
    _val = value'
    _unit = "µV"

  new val unit_nV(value':F64) =>
    _val = value'
    _unit = "nV"

  new val unit_pV(value':F64) =>
    _val = value'
    _unit = "pV"

  new val parse(text:String)? =>
    (_val, _unit) = MetricParser._extract(text)
    match _unit
    | "V" => None
    | "mV" => None
    | "µV" => None
    | "nV" => None
    | "pV" => None
    | "kV" => None
    else error
    end
    
  fun val value():F64 =>
    _val
    
  fun val unit(): String =>
    _unit
    
  fun box string(): String iso^ =>
    (_val.string() + " " + _unit).string()

  fun val to_V():Voltage =>
    match _unit
    | "V" => this
    | "mV" => unit_V(_val * 1E-3)
    | "µV" => unit_V(_val * 1E-6)
    | "nV" => unit_V(_val * 1E-9)
    | "pV" => unit_V(_val * 1E-12)
    | "kV" => unit_V(_val * 1E3)
    else this
    end

  fun val to_kV():Voltage =>
    match _unit
    | "V" => unit_kV(_val * 1E-3)
    | "mV" => unit_kV(_val * 1E-6)
    | "µV" => unit_kV(_val * 1E-9)
    | "nV" => unit_kV(_val * 1E-12)
    | "pV" => unit_kV(_val * 1E-15)
    | "kV" => this
    else this
    end

  fun val to_mV():Voltage =>
    match _unit
    | "V" => unit_mV(_val * 1E3)
    | "mV" => this
    | "µV" => unit_mV(_val * 1E-3)
    | "nV" => unit_mV(_val * 1E-6)
    | "pV" => unit_mV(_val * 1E-9)
    | "kV" => unit_mV(_val * 1E6)
    else this
    end

  fun val to_uV():Voltage =>
    match _unit
    | "V" => unit_uV(_val * 1E6)
    | "mV" => unit_uV(_val * 1E3)
    | "µV" => this
    | "nV" => unit_uV(_val * 1E-3)
    | "pV" => unit_uV(_val * 1E-6)
    | "kV" => unit_uV(_val * 1E9)
    else this
    end

  fun val to_nV():Voltage =>
    match _unit
    | "V" => unit_nV(_val * 1E9)
    | "mV" => unit_nV(_val * 1E6)
    | "µV" => unit_nV(_val * 1E3)
    | "nV" => this
    | "pV" => unit_nV(_val * 1E-3)
    | "kV" => unit_nV(_val * 1E12)
    else this
    end

  fun val to_pV():Voltage =>
    match _unit
    | "V" => unit_nV(_val * 1E12)
    | "mV" => unit_nV(_val * 1E9)
    | "µV" => unit_nV(_val * 1E6)
    | "nV" => unit_nV(_val * 1E3)
    | "pV" => this
    | "kV" => unit_nV(_val * 1E15)
    else this
    end

