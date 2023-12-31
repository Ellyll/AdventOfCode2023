open System.Globalization
open System.Threading

let culture = CultureInfo("en-GB")  
Thread.CurrentThread.CurrentCulture <- culture

Day07_A.run()
Day07_B.run()