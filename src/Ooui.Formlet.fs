namespace Ooui

module Formlets =

  open System.Text.RegularExpressions

  type [<Struct>] Maybe<'T> =
    | Just      of 'T
    | Nothing
  and maybe<'T> = Maybe<'T>

  module Maybe =
    let inline just v   = Just v
    let nothing<'T>     = Nothing

  type FormletContext = FC of unit

  type [<Struct>] FormletChangeNotification = FCN of (unit -> unit)

  type FormletElement = Ooui.Node

  type [<RequireQualifiedAccess>] FormletTree =
    | Empty
    | Element       of FormletElement
    | NestedElement of FormletElement*FormletElement*FormletTree
    | Prepend       of FormletElement*FormletTree
    | Append        of FormletTree*FormletElement
    | Debug         of string*FormletTree
    | Tag           of string*FormletTree
    | Fork          of FormletTree*FormletTree

  type [<Struct>] FormletFailureContext =
    | FFC of (string list)

    with
      member x.Append name : FormletFailureContext =
        let (FFC names) = x
        FFC (name::names)

  type [<RequireQualifiedAccess>] FormletFailureTree =
    | Empty
    | Failure of FormletFailureContext*string
    | Fork    of FormletFailureTree*FormletFailureTree

    static member Join l r : FormletFailureTree =
      match l, r with
      | Empty , Empty -> Empty
      | _     , Empty -> l
      | Empty , _     -> r
      | _     , _     -> Fork (l, r)

    member x.ContextfulFailures () : struct (string*string) [] =
      let ra = ResizeArray 16
      // TODO: Optimize
      let toContext (FFC vs) =
        System.String.Join ('.', vs |> List.rev |> List.toArray)
      let rec loop t =
        match t with
        | Empty -> ()
        | Failure (ffc, msg)  -> ra.Add (struct (toContext ffc, msg))
        | Fork    (l, r)      -> loop l; loop r
      loop x
      ra.ToArray ()

    member x.Failures () : string [] =
      let ra = ResizeArray 16
      let rec loop t =
        match t with
        | Empty -> ()
        | Failure (ffc, msg)  -> ra.Add msg
        | Fork    (l, r)      -> loop l; loop r
      loop x
      ra.ToArray ()

  type [<Struct>] FormletResult<'T> = FR of 'T*FormletFailureTree*FormletTree

  type [<Struct>] Formlet<'T> =
    | FL of (FormletContext -> FormletChangeNotification -> FormletFailureContext -> FormletTree -> FormletResult<'T>)

  module Details =
    let inline fadapt (FL t) = OptimizedClosures.FSharpFunc<_, _, _, _, _>.Adapt t

    let inline finvoke (t : OptimizedClosures.FSharpFunc<_, _, _, _, _>) fc fcn ffc ft = t.Invoke (fc, fcn, ffc, ft)

    let rec findElement (ft : FormletTree) =
      match ft with
      | FormletTree.Element       (:? Element as e)
      | FormletTree.NestedElement (:? Element as e, _, _)
      | FormletTree.Prepend       (:? Element as e, _)    -> Just e
      | FormletTree.Empty
      | FormletTree.Element       _                       -> Nothing
      | FormletTree.NestedElement (_, _, sft)
      | FormletTree.Prepend       (_, sft)
      | FormletTree.Append        (sft, _)
      | FormletTree.Debug         (_, sft)
      | FormletTree.Tag           (_, sft)                -> findElement sft
      | FormletTree.Fork          (lft, rft)              ->
        match findElement lft with
        | Nothing -> findElement rft
        | r       -> r

  open Details

  module Formlet =
    open FSharp.Core.Printf

    let value (v : 'T) : Formlet<'T> =
      FL <| fun fc fcn ffc ft ->
        FR (v, FormletFailureTree.Empty, FormletTree.Empty)

    let failWith (fv : 'T) msg : Formlet<'T>  =
      FL <| fun fc fcn ffc ft ->
        FR (fv, FormletFailureTree.Failure (ffc, msg), FormletTree.Empty)

    let failWithf fv fmt = kprintf (failWith fv) fmt

    let bind (t : Formlet<'T>) (uf : 'T -> Formlet<'U>) : Formlet<'U> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let tft, uft =
          match ft with
          | FormletTree.Fork (tft, uft) -> tft              , uft
          | _                           -> FormletTree.Empty, FormletTree.Empty

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc tft

        let u  = uf tv
        let uf = fadapt u

        let (FR (uv, ufft, uft)) = finvoke uf fc fcn ffc uft

        FR (uv, FormletFailureTree.Join tfft ufft, FormletTree.Fork (tft, uft))

    let apply (f: Formlet<'T -> 'U>) (t : Formlet<'T>) : Formlet<'U> =
      let ff = fadapt f
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let fft, tft =
          match ft with
          | FormletTree.Fork (fft, tft) -> fft              , tft
          | _                           -> FormletTree.Empty, FormletTree.Empty

        let (FR (ff, ffft, fft)) = finvoke ff fc fcn ffc fft
        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc tft

        FR (ff tv, FormletFailureTree.Join ffft tfft, FormletTree.Fork (fft, tft))

    let map m (t : Formlet<'T>) : Formlet<'U> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc ft

        FR (m tv, tfft, tft)

    let debug nm (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let ft =
          match ft with
          | FormletTree.Debug (_, sft)    -> sft
          | _                             -> ft

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc ft

        FR (tv, tfft, FormletTree.Debug (nm, tft))

    let tag nm (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let ft =
          match ft with
          | FormletTree.Tag (snm, sft) when snm = nm  -> sft
          | _                                         -> FormletTree.Empty

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc ft

        FR (tv, tfft, FormletTree.Tag (nm, tft))

    type Builder () =
      member inline x.Bind       (t, uf)   = bind t uf
      member inline x.Return     v         = value v
      member inline x.ReturnFrom t         = t         : Formlet<_>
      member inline x.Zero       ()        = value ()

  let formlet = Formlet.Builder ()

  type Formlet<'T> with
    static member inline (>>=) (t, uf)  = Formlet.bind    t uf
    static member inline (|>>) (t, m)   = Formlet.map     m t
    static member inline (<*>) (f, t)   = Formlet.apply   f t
(*
    static member inline (<|>) (l, r)   = Formlet.orElse  l r
    static member inline (<&>) (l, r)   = Formlet.andAlso l r
*)

  module Validate =
    let validate (validator : 'T -> string maybe) (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let tfr = finvoke tf fc fcn ffc ft
        let (FR (tv, tfft, tft)) = tfr

        match validator tv with
        | Just failure  ->
          FR (tv, FormletFailureTree.Join tfft (FormletFailureTree.Failure (ffc, failure)), tft)
        | Nothing       ->
          tfr

    let notEmpty (t : Formlet<string>) : Formlet<string> =
      let validator (v : string) =
        if v.Length > 0 then
          Nothing
        else
          Just "Must not be empty"
      validate validator t

    let regex (test : Regex) failWith (t : Formlet<string>) : Formlet<string> =
      let validator (v : string) =
        if test.IsMatch v then
          Nothing
        else
          Just failWith
      validate validator t

  module Surround =

    let withElement (creator : unit -> #Element) ``class`` (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let e, ft =
          match ft with
          | FormletTree.NestedElement (:? #Element as e, _, sft) ->
            e, sft
          | _ ->
            let e = creator ()
            e, FormletTree.Empty

        if System.String.IsNullOrEmpty ``class`` |> not then
          e.ClassName <- ``class``

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc ft

        FR (tv, tfft, FormletTree.NestedElement (e, e, tft))

  module Enhance =
    module Details =
      type GroupBox (group : string) =
        inherit Div ()

        let title     = Div ()
        let content   = Div ()
        let titleText = TextNode group

        member x.Initialize () =
          if x.Children.Count = 0 then
            x.ClassName       <- "card"
            title.ClassName   <- "card-header"
            content.ClassName <- "card-body"
            x.AppendChild title           |> ignore
            x.AppendChild content         |> ignore
            title.AppendChild titleText   |> ignore

        member x.Title
          with  get () = titleText.Text
          and   set v  = titleText.Text <- v
        member x.Content  = content

      type Submit (label : string) =
        inherit Div ()

        let header  = GroupBox label
        let content = Div ()

        member x.Initialize () =
          if x.Children.Count = 0 then
            header.Initialize ()
            x.AppendChild header  |> ignore
            x.AppendChild content |> ignore

        member x.Header     = header
        member x.Content    = content

      let regexValidation = Regex ("is-(in)?valid", RegexOptions.CultureInvariant ||| RegexOptions.Compiled ||| RegexOptions.Singleline)

    open Details

    let withClass ``class`` (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let tfr = finvoke tf fc fcn ffc ft
        let (FR (tv, tfft, tft)) = tfr

        match findElement tft with
        | Just element  -> element.ClassName <- ``class``
        | Nothing       -> ()

        tfr

    let withValidation (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let ft, v =
          match ft with
          | FormletTree.Append (FormletTree.Element (:? Input) as ft, v) -> ft, v
          | _ -> FormletTree.Empty, null

        let tfr = finvoke tf fc fcn ffc ft
        let (FR (tv, tfft, tft)) = tfr

        let ntft =
          match tft with
          | FormletTree.Element (:? Input as element)  ->
            let hasError = match tfft with FormletFailureTree.Empty -> false | _ -> true
            let className = element.ClassName
            let classes = className.Split ' '
            let sb = System.Text.StringBuilder (className.Length + 2) // +2 for handling is-valid -> is-invalid
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

    let withLabel label (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let e, ft =
          match ft with
          | FormletTree.Prepend (:? Label as e, sft) ->
            e, sft
          | _ ->
            let e = Label label
            e, FormletTree.Empty

        e.Text <- label

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn (ffc.Append label) ft

        match findElement tft with
        | Just element  -> e.For <- element
        | Nothing       -> ()

        FR (tv, tfft, FormletTree.Prepend (e, tft))

    let withGroupBox label (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let e, ft =
          match ft with
          | FormletTree.NestedElement (:? GroupBox as e, _, sft) ->
            e, sft
          | _ ->
            let e = GroupBox label
            e.Initialize ()
            e, FormletTree.Empty

        e.Title <- label

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn (ffc.Append label) ft

        FR (tv, tfft, FormletTree.NestedElement (e, e.Content, tft))

    let withSubmit (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let e, ft =
          match ft with
          | FormletTree.NestedElement (:? Submit as e, _, sft) ->
            e, sft
          | _ ->
            let e = Submit "All is good"
            e.Initialize ()
            e, FormletTree.Empty

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc ft

        match e.GetLocalAttribute "FormletFailureTree" with
        | (:? FormletFailureTree as fft) when fft = tfft -> ()
        | _ ->
          e.SetLocalAttribute ("FormletFailureTree", box tfft)
          match tfft with
          | FormletFailureTree.Empty ->
            e.Header.ClassName  <- "card text-white bg-success"
            e.Header.Title      <- "Ready to submit!"
            e.Header.Content.ReplaceChildren([|TextNode "All is good!"|])
          | _ ->
            e.Header.ClassName  <- "card text-white bg-danger"
            e.Header.Title      <- "Resolve validation error(s)"
            let list = List false
            let failures = tfft.ContextfulFailures ()
            // TODO: Optimize
            for struct (ctx, msg) in failures do
              let li = ListItem ()
              li.AppendChild (TextNode (sprintf "§ %s: %s" ctx msg)) |> ignore
              list.AppendChild li |> ignore

            e.Header.Content.ReplaceChildren([|list|])

        FR (tv, tfft, FormletTree.NestedElement (e, e.Content, tft))

  module Inputs =

    type [<AbstractClass>] Input<'T> (inputType: InputType) =
      class
        member x.InputType = inputType

        abstract Init   : Input -> unit
        abstract Update : Input -> 'T
      end

    let input (input : Input<'T>) : Formlet<'T> =
      FL <| fun fc fcn ffc ft ->
        let e =
          match ft with
          | FormletTree.Element (:? Input as e) when e.Type = input.InputType ->
            e
          | _ ->
            let e = Input input.InputType
            let (FCN fcn) = fcn
            e.Change.Add (fun _ -> fcn ())
            input.Init e
            e

        let v = input.Update e

        FR (v, FormletFailureTree.Empty, FormletTree.Element e)

    let text placeholder initial : Formlet<string> =
      { new Input<_> (InputType.Text) with
        override x.Init input =
          input.ClassName   <- "form-control"
          input.Value       <- initial
        override x.Update input =
          input.Placeholder <- placeholder
          input.Value
      } |> input

    let checkBox initial : Formlet<bool> =
      { new Input<_> (InputType.Checkbox) with
        override x.Init input =
          input.IsChecked <- initial
        override x.Update input =
          input.IsChecked
      } |> input

  module View =
    let attachTo (t : Formlet<'T>) (node : Node) : unit =
      let rec buildTree (children : ResizeArray<Node>) (ft : FormletTree) =
        let inline add e =
          if isNull e |> not then
            children.Add e
        match ft with
        | FormletTree.Empty ->
          ()
        | FormletTree.Element e ->
          add e
        | FormletTree.NestedElement (e, se, ft) ->
          add e
          buildSubTree se ft
        | FormletTree.Prepend (e, ft) ->
          add e
          buildTree children ft
        | FormletTree.Append (ft, e) ->
          buildTree children ft
          add e
        | FormletTree.Debug (_, ft) ->
          buildTree children ft
        | FormletTree.Tag (_, ft) ->
          buildTree children ft
        | FormletTree.Fork (lft, rft) ->
          buildTree children lft
          buildTree children rft
      and buildSubTree (node : Node) (ft : FormletTree) =
        let nestedChildren = ResizeArray node.Children.Count
        buildTree nestedChildren ft
        node.ReplaceChildren nestedChildren
      let mutable ft = FormletTree.Empty
      let fc = FC ()
      let ffc = FFC []
      let tf = fadapt t
      let rec fcn = FCN (fun () ->
          printfn "Change request"
          update ()
        )
      and update () =
        printfn "FormletTree(Before): %A" ft
        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc ft
        printfn "Formlet Tree(After): %A" tft
        printfn "Value: %A" tv
        printfn "Failure Tree: %A" tfft
        buildSubTree node tft
        ft <- tft
      update ()
