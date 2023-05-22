open Alias

let nes_trainer (chan : In_channel.t) (header : Header.t) (buffer : Bytes.t) =
  let trainer_buff = Bytes.make 0x200 '\x00' in
  if header.flags_6.trainer_present then
    match In_channel.really_input chan trainer_buff 0 0x200 with
    | Some () -> ()
    | None -> failwith "Trainer signified in bit 6 flags but not present"
  else ()