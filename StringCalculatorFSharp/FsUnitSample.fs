module StringCalculatorFSharp.Tests

open NUnit.Framework
open FsUnit
open System.Text.RegularExpressions

let parseInt(x)=int(string(x))

let rec add_rec(x, acc)=
    match x with
    | head :: tail when tail.IsEmpty=false -> add_rec(tail, (System.Int32.Parse(head.ToString()))+acc)
    | head :: tail  -> (System.Int32.Parse(head.ToString()))+acc
    | [] -> acc

let add(x:string) = 
    let delimeter = if(x.StartsWith("//")) then x.Substring(2, 1) else ""
    let regex = Regex("[//\n,"+delimeter+"]")
    if (x="") then 0
    else add_rec(regex.Split(x)|>Seq.toList|>List.filter (fun x -> not (x="")), 0)

[<TestFixture>] 
type ``String calculation`` ()=
    [<Test>] member test.``Empty string returns 0``() = add("") |> should equal 0

    [<Test>] member test.``One number returns itself``() =
        add("1") |> should equal 1
    
    [<Test>] member test.``Two number should be added``() =
        add("1,2") |> should equal 3

    [<Test>] member test.``Should add any amount of numbers``() =
        add("1,2,3,4") |> should equal 10
        add("1,2,3,4,5") |> should equal 15

    [<Test>] member test.``Separator can be newline``() =
        add("1\n2,3") |> should equal 6
    
    [<Test>] member test.``Separator can be given in first line``() =
        add("//;\n1;2") |> should equal 3
    
