let List/map =
      https://prelude.dhall-lang.org/v11.1.0/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
let replicate = https://prelude.dhall-lang.org/List/replicate
let seq : Natural -> List Natural =
          \(count : Natural)
      ->  let indexed = List/indexed Natural (replicate count Natural 1)
          let IndexedType = { index : Natural, value : Natural }
          in  List/map
                IndexedType
                Natural
                (\(index : IndexedType) -> index.index + 1)
                indexed
let flatten : forall (a : Type) -> List (List a) -> List a =
        \(a : Type) -> \(ll : List (List a)) -> List/fold (List a) ll (List a) (λ(x : List a) → λ(y : List a) → x # y) ([]: List a)
     
let Duration : Type =
    < Millis : Natural >
let DelayCommand : Type =
    {
        duration : Natural
    }
let Color : Type =
    {
        r : Natural,
        g : Natural,
        b : Natural,
        brightness : Optional Double
    }
let defColor : Color = { r = 0, g = 0, b = 0, brightness = None Double }
let OnCommand : Type =
    {
        led : Natural,
        color : Color,
    }
let OffCommand : Type =
    {
        led : Natural,
    }
let Command : Type =
    < Delay : DelayCommand 
    | On : OnCommand
    | Off : OffCommand>
let TypedCommand : Type = { instr: Text, val: Command }

let SwitchOn : Natural -> Color -> TypedCommand = \(led : Natural) -> \(color : Color) ->  { instr = "On", val = Command.On { led = led, color = color }}
let SwitchOff : Natural -> TypedCommand = \(led : Natural) -> { instr = "Off", val = Command.Off { led = led }}
let Delay : Natural -> TypedCommand = \(millis : Natural) -> { instr = "Delay", val = Command.Delay { duration = millis }}

let allLeds : List Natural = [0, 1, 2, 3, 4, 5, 6, 7]
let MapLeds : (Natural -> TypedCommand) -> (List TypedCommand) = \(f : Natural -> TypedCommand) -> List/map Natural TypedCommand f allLeds
let MapSeq :  forall (a : Type) -> Natural -> (Natural -> a) -> List a = \(a : Type) -> \(n : Natural) -> \(f : (Natural -> a)) -> List/map Natural a f (seq n)
in {
        Duration = Duration,
        DelayCommand = DelayCommand,
        Color = Color,
        defColor = defColor,
        OnCommand = OnCommand, 
        OffCommand = OffCommand,
        Command = Command,
        TypedCommand = TypedCommand,
        SwitchOn = SwitchOn,
        SwitchOff = SwitchOff,
        Delay = Delay,
        MapLeds = MapLeds,
        seq = seq,
        flatten = flatten,
        MapSeq = MapSeq,
    }

