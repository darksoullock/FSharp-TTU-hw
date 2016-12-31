(*** hide ***)
#load "packages/FsLab/Themes/DefaultWhite.fsx"
#load "packages/FsLab/FsLab.fsx"

(**

Droughts, floods, extreme temperatures
========================

In this document we will analyze how much people were affected by natural disasters and related to this stuff.

*)

(*** hide ***)
open FSharp.Data
open Deedle
open System
open System.IO
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle

let wb = WorldBankData.GetDataContext()

let getIndicator (c:WorldBankData.ServiceTypes.Country) = 
        c.Indicators.``Droughts, floods, extreme temperatures (% of population, average 1990-2009)``

let getName (c:WorldBankData.ServiceTypes.Country) = c.Name

let headOrDefault def (s:seq<'a>) = 
    let value = Seq.tryHead s
    if value = None then def else Option.get value

let countriesWithData = Seq.filter (getIndicator >> (fun i -> i.Values.Count > 0)) wb.Countries

let selectedCountries = Seq.map (fun (i:WorldBankData.ServiceTypes.Country) -> i.Name) countriesWithData |> List.ofSeq

let indexOfOrEnd (ch:char) (index:int) (str:string) = 
    try
        str.IndexOf(ch, index) 
    with
    | :? ArgumentOutOfRangeException -> str.Length

let formatParagraph (lineWidth:int) (str:string) : string =    
    let rec worker i = 
        if i >= str.Length
        then []
        else
            let space = indexOfOrEnd ' ' (i+lineWidth) str
            str.Substring(i, space-i) :: (worker space)
    String.Join(Environment.NewLine, worker 0)

(**
Here is some info about this indicator
*)
(*** define-output:about ***)
wb.Topics.Environment.Indicators.``Droughts, floods, extreme temperatures (% of population, average 1990-2009)``.Description |> formatParagraph 120

(*** include-it:about ***)


(**

Let's have a look at the list of countries we analyzed (have data for):
*)
(*** define-output:countries ***)
String.Join(Environment.NewLine, selectedCountries |> Array.ofList)
(*** include-it:countries ***)

(*** hide ***)
let amountAffected = Seq.map (fun (i:WorldBankData.ServiceTypes.Country) -> (i, (getIndicator i).Values |> Seq.head)) countriesWithData |> List.ofSeq |> List.sortByDescending snd

(**
####The top 14 of the most affected by disasters countries are:
*)

(*** hide ***)
let top14 = amountAffected |> List.take 14
(*** define-output:chart1 ***)
top14 |> List.map (fun i -> (i |> fst |> getName, snd i)) |> Chart.Column |> Chart.WithLabels ["% of population"]
(*** include-it:chart1 ***)

(**

#####So think twice, 
before deciding travel to __London__, __Paris__, __Poland__ or __Banana__ (towns at the __Kiribati__ island, #12 in our rating)

![](http://65.media.tumblr.com/741dc8d9307f13ee49dd2a44a0d07196/tumblr_o38ahggLk51rkeu7wo1_400.jpg)

*)


(**
#####Lets see those countries efforts to reduce number of disasters
*)

(*** hide ***)
let reduction = top14 |> List.map fst |> List.map (fun i -> (i.Name, headOrDefault -0.0 i.Indicators.``Disaster risk reduction progress score (1-5 scale; 5=best)``.Values))
let reductionInputs = [List.map (fun i -> (i |> fst |> getName, snd i)) top14 ; reduction]
let series2 = [|Series("bars"); Series("bars")|]

(*** define-output:chart2 ***)
reductionInputs 
    |> Chart.Combo
    |> Chart.WithOptions 
         (Options(title = "The most dangerous countries with their effort of disasters reduction", series = 
            series2))
    |> Chart.WithLabels ["% of population";"Disaster risk reduction progress score (1-5 scale; 5=best)"]
(*** include-it:chart2 ***)

(**

#####I have labeled this countries on the map, 
#####and as we can see, all the countries (except Tajikistan) are in Africa (probably suffer from drough) or near the sea (floods)

*)
(*** hide ***)
let mutable index = 15
let area = top14 |> List.map fst |> List.map (fun i -> index <- index-1; (i.Name, index))
(*** define-output:chart3 ***)
area|> Chart.Geo
(*** include-it:chart3 ***)

(** 

#####And probably they are not the rich coutries. At the next chart we can see their GDP per capita (and Estonian for comparison)

*)
let gdp = (wb.Countries.Estonia.Name, headOrDefault 0.0 wb.Countries.Estonia.Indicators.``GDP per capita (current US$)``.Values) :: 
    (top14 |> List.map fst |> List.map (fun i -> (i.Name, headOrDefault -0.0 i.Indicators.``GDP per capita (current US$)``.Values)))
(*** define-output:chart4 ***)
gdp |> Chart.Pie |> Chart.WithLabels ["GDP per capita (current US$)"]
(*** include-it:chart4 ***)

(**

#####Finally, let's take a look at the all countries with data. May be they all have the similar values?
On the left of line are our top 14 countries

*)

(*** define-output:chart5 ***)
[amountAffected |> List.map (fun (i, j) -> i.Name, j); top14|> List.last |> (fun i -> [i |> fst |> getName, 10.0])] |> Chart.Combo
    |> Chart.WithOptions 
         (Options(title = "The most dangerous countries with their effort of disasters reduction", series = 
            series2))
(*** include-it:chart5 ***)


