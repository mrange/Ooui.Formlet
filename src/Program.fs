open System
open Ooui

open Ooui.Formlets

let simple () =
  let p = Paragraph "Hello"

  let button = Button "Click me!"

  let input = Input InputType.Text
  let div = Div [|input :> Element; p :> Element; button :> Element|]

  let mutable count = 0

  button.Click.Add (fun _ ->
      count <- count + 1
      printfn "%A" input.Value
      button.Text <- sprintf "Clicked %d times!" count
      p.Text <- "GG"
      let pp = Paragraph "GG2"
      div.AppendChild (pp :> Node) |> ignore
      ()
    )

  ()
[<EntryPoint>]
let main argv =
  
  UI.Port <- 8800

  let form = Form ()

  let fr = Test.test form

  printfn "%A" fr
  
  UI.Publish ("/formlet", form)

  Console.ReadLine () |> ignore

  0
