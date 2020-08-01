

trait Metric is Float
    new val parse(text:String)?
    fun val value():F64
    fun val unit():String
    
    
primitive MetricParser
    fun val parse(text:String):Metric val? =>
        var result: Metric val
        result = Acceleration.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Angle.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Area.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Current.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Density.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Distance.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Energy.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Flow.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Force.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Frequency.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Gravity.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Humidity.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Mass.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Power.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Pressure.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Ratio.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Resistance.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Temperature.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Periodicity.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Velocity.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Voltage.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result = Volume.parse(text)?
        if( not (result is None ) ) then
            return result
        end
        result
        
    fun _extract(t:String):(F64,String) =>
      try
        let pos = t.find(" ")?
        try
          let value = t.substring(0,pos).f64()?
          let unit = t.substring(pos+1)
          (value,unit)
        else
          (0.0, "")
        end
     else
       // loop backward until digit, use that as break
       // TODO but let's wait with that...
       (0.0,"")
     end
