
open System.Globalization
open System.Threading

// Work around bug when using cy-GB
let culture = CultureInfo("en-GB")  
Thread.CurrentThread.CurrentCulture <- culture

Day01_A.run()
Day01_B.run()