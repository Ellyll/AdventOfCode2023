
open System.Globalization
open System.Threading

// Work around issue with e.g. "ng" being treated as a single letter when using StartsWith() when using cy-GB
let culture = CultureInfo("en-GB")  
Thread.CurrentThread.CurrentCulture <- culture

Day02_A.run()
Day02_B.run()