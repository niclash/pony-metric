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

class val Ratio
  let _value:F64
  let _unit:String
  
  new val unit_percent(value':F64) =>
    _value = value'
    _unit = "%"

  new val unit_fraction(value':F64) =>
    _value = value'
    _unit = ""

  fun val value():F64 =>
    _value
    
  fun val unit(): String =>
    _unit
    
  fun val string(): String =>
    _value.string() + " " + _unit

  fun val to_percent():Ratio =>
    match _unit
    | "%" => this
    | "" => unit_percent(_value * 100)
    else this
    end

  fun val to_fraction():Ratio =>
    match _unit
    | "%" => unit_fraction(_value / 100)
    | "" => this
    else this
    end
