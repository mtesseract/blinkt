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
    }
let defColor : Color = { r = 0, g = 0, b = 0 }
let SetColorCommand : Type =
    {
        led : Natural,
        color : Color,
    }
let RedCommand : Type =
    {
        led : Natural,
        delta : Integer,
    }
let SetRedCommand : Type =
    {
        led : Natural,
        intensity : Natural,
    }
let GreenCommand : Type =
    {
        led : Natural,
        delta : Integer,
    }
let SetGreenCommand : Type =
    {
        led : Natural,
        intensity : Natural,
    }
let BlueCommand : Type =
    {
        led : Natural,
        delta : Integer,
    }
let SetBlueCommand : Type =
    {
        led : Natural,
        intensity : Natural,
    }
let SetBrightnessCommand : Type =
    {
        led : Natural,
        brightness : Double,
    }
let BrightnessCommand : Type =
    {
        led : Natural,
        delta : Double,
    }
let OnCommand : Type =
    {
        led : Natural,
    }
let OffCommand : Type =
    {
        led : Natural,
    }
let Command : Type =
    < Delay : DelayCommand 
    | On : OnCommand
    | Off : OffCommand
    | SetColor : SetColorCommand
    | SetBrightness : SetBrightnessCommand
    | Brightness : BrightnessCommand
    | Red : RedCommand
    | SetRed : SetRedCommand
    | Green : GreenCommand
    | SetGreen : SetGreenCommand
    | Blue : BlueCommand
    | SetBlue : SetBlueCommand
    >
let TypedCommand : Type = { instr: Text, val: Command }
let SetColor : Natural -> Color -> TypedCommand = \(led : Natural) ->  \(color : Color) -> { instr = "SetColor", val = Command.SetColor { led = led, color = color }}
let Red : Natural -> Integer -> TypedCommand = \(led : Natural) -> \(delta : Integer) -> { instr = "Red", val = Command.Red { led = led, delta =  delta }}
let SetRed : Natural -> Natural -> TypedCommand = \(led : Natural) -> \(intensity : Natural) -> { instr = "SetRed", val = Command.SetRed { led = led, intensity = intensity }}
let Green : Natural -> Integer -> TypedCommand = \(led : Natural) -> \(delta : Integer) -> { instr = "Green", val = Command.Green { led = led, delta =  delta }}
let SetGreen : Natural -> Natural -> TypedCommand = \(led : Natural) -> \(intensity : Natural) -> { instr = "SetGreen", val = Command.SetGreen { led = led, intensity = intensity }}
let Blue : Natural -> Integer -> TypedCommand = \(led : Natural) -> \(delta : Integer) -> { instr = "Blue", val = Command.Blue { led = led, delta =  delta }}
let SetBlue : Natural -> Natural -> TypedCommand = \(led : Natural) -> \(intensity : Natural) -> { instr = "SetBlue", val = Command.SetBlue { led = led, intensity = intensity }}
let SetBrightness : Natural -> Double -> TypedCommand = \(led : Natural) ->  \(brightness : Double) -> { instr = "SetBrightness", val = Command.SetBrightness { led = led, brightness = brightness }}
let Brightness : Natural -> Double -> TypedCommand = \(led : Natural) -> \(delta : Double) -> { instr = "Brightness", val = Command.Brightness { led = led, delta =  delta }}
let On : Natural -> TypedCommand = \(led : Natural) ->  { instr = "On", val = Command.On { led = led }}
let Off : Natural -> TypedCommand = \(led : Natural) -> { instr = "Off", val = Command.Off { led = led }}
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
        On = On,
        Off = Off,
        SetColor = SetColor,
        Red = Red,
        SetRed = SetRed,
        Green = Green,
        SetGreen = SetGreen,
        Blue = Blue,
        SetBlue = SetBlue,
        SetBrightness = SetBrightness,
        Brightness = Brightness,
        Delay = Delay,
        MapLeds = MapLeds,
        seq = seq,
        flatten = flatten,
        MapSeq = MapSeq,
    }

