module Ooui.Formlets.Bootstrap

open Ooui
open Ooui.Formlets.Core
open Ooui.Formlets.Core.Inputs

open System.Text.RegularExpressions


module Details =
  type UnnamedGroupBox () =
    inherit Div ()

    let content   = Div ()

    member x.Initialize () =
      if x.Children.Count = 0 then
        x.ClassName       <- "card mb-3"
        content.ClassName <- "card-body"
        x.AppendChild content |> ignore

    member x.Content  = content

  type GroupBox (group : string) =
    inherit Div ()

    let header    = Div ()
    let content   = Div ()

    member x.Initialize () =
      if x.Children.Count = 0 then
        x.ClassName         <- "card mb-3"
        header.ClassName    <- "card-header"
        content.ClassName   <- "card-body"
        x.AppendChild header          |> ignore
        x.AppendChild content         |> ignore

    member x.Header   = header
    member x.Content  = content

  type Submit (fc : FormletContext, label : string) =
    inherit Div ()

    let submit  = Button ()
    let reset   = Button ()
    let text    = Span ()

    let legend  = GroupBox label
    let content = Div ()

    member x.Initialize () =
      if x.Children.Count = 0 then
        legend.Initialize ()

        submit.Type               <- ButtonType.Button
        submit.Text               <- "Submit"
        submit.Click.Add          fc.SubmitNotification
        submit.ClassName          <- "btn btn-dark"
        submit.Style.MarginRight  <- "8px"

        reset.Type                <- ButtonType.Button
        reset.Text                <- "Reset"
        reset.Click.Add           fc.ResetNotification
        reset.ClassName           <- "btn btn-warning"
        reset.Style.MarginRight   <- "8px"

        text.Style.FontSize       <- "larger"

        legend.Header.AppendChild submit  |> ignore
        legend.Header.AppendChild reset   |> ignore
        legend.Header.AppendChild text    |> ignore

        x.AppendChild legend              |> ignore
        x.AppendChild content             |> ignore

    member x.Legend     = legend
    member x.LegendText = text
    member x.Content    = content

  let regexValidation = Regex ("is-(in)?valid", RegexOptions.CultureInvariant ||| RegexOptions.Compiled ||| RegexOptions.Singleline)

open Details

module Enhance =

  let withValidation (t : Formlet<'T>) : Formlet<'T> =
    let tf = fadapt t
    FL <| fun fc ffc ft ->

      let ft, v =
        match ft with
        | FormletTree.Append (FormletTree.Element (:? Input) as ft, v) -> ft, v
        | _ -> FormletTree.Empty, null

      let tfr = finvoke tf fc ffc ft
      let (FR (tv, tfft, tft)) = tfr

      let ntft =
        match tft with
        | FormletTree.Element (:? Input as element)  ->
          let hasError  = match tfft with FormletFailureTree.Empty -> false | _ -> true
          let className = element.ClassName
          let classes   = className.Split ' '
          let sb        = System.Text.StringBuilder (className.Length + 2) // +2 for handling is-valid -> is-invalid
          let inline append (v : string) =
            if sb.Length > 0 then
              sb.Append ' ' |> ignore
            sb.Append v |> ignore
          for ``class`` in classes do
            if ``class``.Length = 0 then
              ()
            elif regexValidation.IsMatch ``class`` |> not then
              append ``class``
          if hasError then
            append "is-invalid"
          else
            append "is-valid"

          element.ClassName <- sb.ToString ()

          let vv =
            if hasError then
              let div =
                match v with
                | :? Div as div -> div
                | _ -> Div ()
              div.ClassName <- "invalid-feedback"

              let sb = System.Text.StringBuilder 16
              let inline append (v : string) =
                if sb.Length > 0 then
                  sb.Append "; " |> ignore
                sb.Append v |> ignore
              for msg in tfft.Failures () do
                append msg

              let text = sb.ToString ()

              if text.Length > 0 then
                div.Text <- text
              else
                div.Text <- "Invalid input."

              div
            else
              null

          FormletTree.Append (tft, vv)
        | _ ->
          FormletTree.Append (tft, null)

      FR (tv, tfft, ntft)

  let withLabeledBox label (t : Formlet<'T>) : Formlet<'T> =
    let tf = fadapt t
    FL <| fun fc ffc ft ->

      let e, ft =
        match ft with
        | FormletTree.NestedElement (:? GroupBox as e, _, sft) ->
          e, sft
        | _ ->
          let e = GroupBox label
          e.Initialize ()
          e, FormletTree.Empty

      e.Header.Text <- label

      let (FR (tv, tfft, tft)) = finvoke tf fc (ffc.Append label) ft

      FR (tv, tfft, FormletTree.NestedElement (e, e.Content, tft))

  let withBox (t : Formlet<'T>) : Formlet<'T> =
    let tf = fadapt t
    FL <| fun fc ffc ft ->

      let e, ft =
        match ft with
        | FormletTree.NestedElement (:? UnnamedGroupBox as e, _, sft) ->
          e, sft
        | _ ->
          let e = UnnamedGroupBox ()
          e.Initialize ()
          e, FormletTree.Empty

      let (FR (tv, tfft, tft)) = finvoke tf fc ffc ft

      FR (tv, tfft, FormletTree.NestedElement (e, e.Content, tft))

  let withSubmit (t : Formlet<'T>) : Formlet<'T> =
    let tf = fadapt t
    FL <| fun fc ffc ft ->

      let e, ft =
        match ft with
        | FormletTree.NestedElement (:? Submit as e, _, sft) ->
          e, sft
        | _ ->
          let e = Submit (fc, "All is good")
          e.Initialize ()
          e, FormletTree.Empty

      let (FR (tv, tfft, tft)) = finvoke tf fc ffc ft

      match e.GetLocalAttribute "FormletFailureTree" with
      | (:? FormletFailureTree as fft) when fft = tfft -> ()
      | _ ->
        e.SetLocalAttribute ("FormletFailureTree", box tfft)
        match tfft with
        | FormletFailureTree.Empty ->
          e.Legend.ClassName        <- "card mb-3 text-white bg-success"
          e.LegendText.Text         <- "Ready to submit!"
          e.Legend.Content.ReplaceChildren [|TextNode "All is good!"|]
        | _ ->
          e.Legend.ClassName        <- "card mb-3 text-white bg-danger"
          e.LegendText.Text         <- "Resolve validation error(s)"
          let list = List false
          let failures = tfft.ContextfulFailures ()
          // TODO: Optimize
          for struct (ctx, msg) in failures do
            let li = ListItem ()
            li.AppendChild (TextNode (sprintf "§ %s: %s" ctx msg)) |> ignore
            list.AppendChild li |> ignore

          e.Legend.Content.ReplaceChildren [|list|]

      FR (tv, tfft, FormletTree.NestedElement (e, e.Content, tft))

module Inputs =
  let checkBox initial : Formlet<bool> =
    { new Input<_> (InputType.Checkbox) with
      override x.Init input =
        input.ClassName   <- "form-check-input"
        input.IsChecked   <- initial
      override x.Update input =
        input.IsChecked
    } |> input

  let select (options : (string*'T) []) : Formlet<'T> =
    if options.Length = 0 then
      failwith "select - expected at least one choice"
    FL <| fun fc ffc ft ->
      let e =
        match ft with
        | FormletTree.Element (:? Select as e) ->
          e
        | _ ->
          let e = Select ()
          e.ClassName <- "form-control"
          e.Change.Add fc.ChangeNotification
          e

      match e.GetLocalAttribute "options" with
      | :? ((string*'T) []) as existingOptions when refEq existingOptions options ->
        ()
      | _ ->
        e.SetLocalAttribute ("options", options)
        e.ReplaceChildren [||]
        // TODO: Ensure no object created
        let rec loop i =
          if i < options.Length then
            let label, _ = options.[i]
            e.AddOption (label, i.ToString ())
            loop (i + 1)
        loop 0

      let b, v = System.Int32.TryParse e.Value

      if b then
        FR (options.[v] |> snd, FormletFailureTree.Empty, FormletTree.Element e)
      else
        FR (options.[0] |> snd, FormletFailureTree.Failure (ffc, "No value selected"), FormletTree.Element e)

  let text placeholder initial : Formlet<string> =
    { new Input<_> (InputType.Text) with
      override x.Init input =
        input.Value       <- initial
        input.ClassName   <- "form-control"
      override x.Update input =
        input.Placeholder <- placeholder
        input.Value
    } |> input
