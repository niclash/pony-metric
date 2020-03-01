# pony-metric

Library for physical dimensions, i.e. mainly SI units and conversions between them


## Work in progress
This library is not complete and constantly evolving, seeking to add more dimensions,
more units and more conversions. Please submit Pull Requests.

### Missing low hanging fruit
* `add()`, `sub()`, `div()` and `mul()` operators for all. See `energy.pony` for example.

* Formulas, such a `F = m * a`, so that conversion can be done easily. See `energy.pony` for example.

* Common Imperial units, such as `mph` and`ft`.


## Example
```
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
```

Output:
```
1000 kWh
3.6e+09 J
3.6 GJ
3.6e+09 Nm
1e+06 Wh
1 MWh
1900 kWh
100 kWh
-0.1 MWh
84420 kJ
```
