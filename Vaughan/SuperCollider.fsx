#r "../packages/bespoke-osc-library/1.0.0/lib/Bespoke.Common.Osc.dll"

open System
open System.Net
open Bespoke.Common.Osc
OscPacket.LittleEndianByteOrder <- false

let superCollider = new IPEndPoint(IPAddress.Loopback, 57110)
let superColliderLanguage = new IPEndPoint(IPAddress.Loopback, 57120)

let id = ref 1
let nextId = (fun () -> id := !id + 1; !id)

let private tofloat32 (f : float) = f |> Convert.ToSingle

type level = float
type attack = float
type release = float
type curve = float
type proportion = float

let stop() =
    let msg = new OscMessage(superCollider, "/g_freeAll")
    msg.Append(0) |> ignore
    msg.Send(superCollider) |> ignore

let private sinOsc freq =
    let id = nextId()

    let msg = new OscMessage(superCollider, "/s_new")
    msg.Append("default") |> ignore
    msg.Append(id) |> ignore
    msg.Append(1) |> ignore
    msg.Append(0) |> ignore
    msg.Append("freq") |> ignore
    msg.Append(tofloat32 freq) |> ignore
    msg.Send(superCollider)

sinOsc 440.0

stop()