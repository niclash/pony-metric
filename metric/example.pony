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

actor Main
  new create( env:Env ) =>
    let a = Energy.unit_kWh(1000)
    
    env.out.print(a.string())
    env.out.print(a.to_J().string())
    env.out.print(a.to_GJ().string())
    env.out.print(a.to_Nm().string())
    env.out.print(a.to_Wh().string())
    env.out.print(a.to_MWh().string())
    
    let b = Energy.unit_MWh(0.9)

    let c = a + b
    env.out.print(c.string())
    
    let d = a - b
    env.out.print(d.string())

    let e = b - a
    env.out.print(e.string())

    try
      let f = Energy.parse("23.45 kWh")?.to_kJ()
      env.out.print(f.string())
    else
      env.out.print("Can't parse.")
    end
