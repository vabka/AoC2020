#load "../common.fsx"

type IntRange = { Min: int; Max: int }
type TicketPolicy = {Field: string; Ranges: IntRange list;}

let parsePolicy =0
open Common
open System