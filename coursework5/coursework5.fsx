(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: Working with data: intro to type providers and charting

  ------------------------------------
  Name:
  TUT Student ID:
  ------------------------------------


  Answer the questions below.  You answers to questions 1--6 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 29, 2016.
*)

// 1) The following three different fuel consumption units for vehicles are in use:
//    * liters per 100 km
//    * miles per imperial gallon (in use in the UK)
//    * miles per US gallon (in use in the US)
// 1.a) Define the units in terms of units of measure.
// 1.b) Define 2 functions that convert the appropriate US and imperial mpg values to
//      liters per 100 km. 
// 1.c) Define a function that converts liters per 100 km of appropriate fuel to
//      CO2 emissions g per km.

[<Measure>] type l      // liters
[<Measure>] type km     // 1 km
[<Measure>] type hkm     // 100 km
[<Measure>] type m      // miles
[<Measure>] type img    // imperial gallon
[<Measure>] type usg    // US gallon
[<Measure>] type g      // gram

let liters100km = 10.0<l/hkm>
let milesPerImperialGallon = 10.0<m/img>
let milesPerUSGallon = 10.0<m/usg>

let kmInMile = 1.60934<km/m>
let kmInHkm = 100.0<km/hkm>
let litersInImG = 4.54609<l/img>
let litersInUSG = 3.785<l/usg>


let milesPerImperialGallonToLitersPer100Km (mpg : float<m/img>) : float<l/hkm> = 
    1.0/(mpg*kmInMile/litersInImG)*kmInHkm

//milesPerImperialGallonToLitersPer100Km 40.0<m/img>


let milesPerUSGallonToLitersPer100Km (mpg : float<m/usg>) : float<l/hkm> = 
    1.0/(mpg*kmInMile/litersInUSG)*kmInHkm
    
//milesPerUSGallonToLitersPer100Km 40.0<m/usg>


let grammsCO2PerDieselFuelLiter = 2680.0<g/l>           // https://people.exeter.ac.uk/TWDavies/energy_conversion/Calculation%20of%20CO2%20emissions%20from%20fuels.htm
let grammsCO2PerPetrolFuelLiter = 2310.0<g/l>
let grammsCO2PerHydrohenLiter = 0.0<g/l>

let consumptionToCO2 (grammsCO2PerFuelLiter : float<g/l>) (c : float<l/hkm>) : float<g/km> = 
    c*grammsCO2PerFuelLiter/kmInHkm

let consumptionPetrolToCO2 = consumptionToCO2 grammsCO2PerPetrolFuelLiter
let consumptionDieselToCO2 = consumptionToCO2 grammsCO2PerDieselFuelLiter
let consumptionHydrohenToCO2 = consumptionToCO2 grammsCO2PerHydrohenLiter

//consumptionPetrolToCO2 8.0<l/hkm> 
//consumptionDieselToCO2 6.0<l/hkm> 
//consumptionHydrohenToCO2 666.0<l/hkm> 

// 2) Get the fuel consumption data
// 2.a) in imperial MPG (miles per imperial gallon) of at least 20 vehicles from
// http://carfueldata.direct.gov.uk/search-by-fuel-economy.aspx
// Save the data in file called imperial.csv

// 2.b) Get the fuel consumption data of at least 20 cars in US MPG (miles per US gallon) from
// https://www.fueleconomy.gov/feg/download.shtml
// save the data in file called us.csv

// 3) Load the imperial.csv and us.csv files using FSharp.Data.CsvProvider<>

#r "packages\\FSharp.Data.dll"
#r "packages\\FSharp.Charting.dll"

open FSharp.Data
open FSharp.Data.CsvExtensions

open System.Linq
let libPath = System.AppDomain.CurrentDomain.GetAssemblies().First(fun i -> i.FullName.Contains("Data")).CodeBase.Substring(8)  // path to FSharp.Data.DesignTime (./packages)
let dir = System.IO.Path.GetDirectoryName(libPath + "..")   // getting script directory from path
System.IO.Directory.SetCurrentDirectory(dir)                // setting current directory to script location instead of %TEMP%

type ImperialProvider = CsvProvider<"imperial.csv">
let imperial = ImperialProvider.Load("imperial.csv")

type USProvider = CsvProvider<"us.csv">
let us = USProvider.Load("us.csv")

// 4) Write a function to convert the appropriate mpg data into
//    liters per 100 km using the functions defined in Q1.

let irows = imperial.Rows |> List.ofSeq
let usrows = us.Rows |> List.ofSeq


// CO2 g/km Metric Urban (Cold) -- Imperial
// co2TailpipeGpm  UCity        -- US

let rec convertDataImperial (rows:ImperialProvider.Row list) = 
    match rows with
    | [] -> []
    | x::xs -> (1.0<l/hkm> * x.``Metric Urban (Cold)``, 1.0<g/km> * float x.``CO2 g/km``) :: (convertDataImperial xs)

let rec convertDataUS (rows:USProvider.Row list) = 
    match rows with
    | [] -> []
    | x::xs -> (milesPerUSGallonToLitersPer100Km (1.0<m/usg>*(float x.UCity)), 1.0<g/m>*float x.Co2TailpipeGpm/kmInMile) :: (convertDataUS xs)

// 5) Display the converted data in an appropriate chart (select the type that is most 
//    appropriate for displaying the data).

open FSharp.Charting

let iFilteredData = convertDataImperial irows
let c1 = Chart.Column (iFilteredData, Title = "Imperial data", XTitle = "CO2", YTitle = "Liters/100km")
c1.ShowChart()

let usFilteredData = convertDataUS usrows
let c2 = Chart.Column (usFilteredData, Title = "US data", XTitle = "CO2", YTitle = "Liters/100km")
c2.ShowChart()


irows.Length
usrows.Length

// 6) Combine the data from 2 data sources into a single chart. Add appropriate titles and
//    legends. 

let c3 = Chart.Combine([Chart.Column (iFilteredData);
                        Chart.Column (usFilteredData, Title = "US (red) and Imperial (Blue) data", Color = System.Drawing.Color.Red, XTitle = "Fuel consumption (l/100km)", YTitle = "CO2")])
                        .WithXAxis(Title = "Fuel consumption (l/100km)").WithYAxis(Title = "CO2 Exhaust (g/l)")
c3.ShowChart()


let c4 = Chart.Column (iFilteredData@usFilteredData, Title = "US (red) and Imperial (Blue) data", Color = System.Drawing.Color.Gray, YTitle = "CO2", XTitle = "Liters/100km" )
c4.ShowChart()



