module Ooui.Formlets.Core

open Ooui

open System.Text.RegularExpressions

type [<Struct>] Maybe<'T> =
  | Just      of 'T
  | Nothing
and maybe<'T> = Maybe<'T>

module Maybe =
  let inline just v = Just v
  let nothing<'T>   = Nothing

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
  let inline refEq l r = System.Object.ReferenceEquals (l, r)

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

  let andAlso (t: Formlet<'T>) (u : Formlet<'U>) : Formlet<'T*'U> =
    let tf = fadapt t
    let uf = fadapt u
    FL <| fun fc fcn ffc ft ->

      let tft, uft =
        match ft with
        | FormletTree.Fork (tft, uft) -> tft              , uft
        | _                           -> FormletTree.Empty, FormletTree.Empty

      let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc tft
      let (FR (uv, ufft, uft)) = finvoke uf fc fcn ffc uft

      FR ((tv, uv), FormletFailureTree.Join tfft ufft, FormletTree.Fork (tft, uft))

  let unwrap (t : Formlet<Formlet<'T>>) : Formlet<'T> =
    let tf = fadapt t
    FL <| fun fc fcn ffc ft ->

      let tft, uft =
        match ft with
        | FormletTree.Fork (tft, uft) -> tft              , uft
        | _                           -> FormletTree.Empty, FormletTree.Empty

      let (FR (u, tfft, tft)) = finvoke tf fc fcn ffc tft

      let uf = fadapt u

      let (FR (uv, ufft, uft)) = finvoke uf fc fcn ffc uft

      FR (uv, FormletFailureTree.Join tfft ufft, FormletTree.Fork (tft, uft))

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
        | FormletTree.Tag (snm, sft) when snm.Equals nm -> sft
        | _                                             -> FormletTree.Empty

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
  static member inline (<&>) (l, r)   = Formlet.andAlso l r

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
  let withClass ``class`` (t : Formlet<'T>) : Formlet<'T> =
    let tf = fadapt t
    FL <| fun fc fcn ffc ft ->

      let tfr = finvoke tf fc fcn ffc ft
      let (FR (tv, tfft, tft)) = tfr

      match findElement tft with
      | Just element  -> element.ClassName <- ``class``
      | Nothing       -> ()

      tfr

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
    let mutable ft  = FormletTree.Empty
    let fc          = FC ()
    let ffc         = FFC []
    let tf          = fadapt t
    let rec fcn     = FCN (fun () ->
        printfn "Change request"
        update ()
      )
    and update ()   =
      printfn "FormletTree(Before): %A" ft
      let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc ft
      printfn "Formlet Tree(After): %A" tft
      printfn "Value: %A" tv
      printfn "Failure Tree: %A" tfft
      buildSubTree node tft
      ft <- tft
    update ()

