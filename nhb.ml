open Lwt
open Lwt_unix
module Irc = Irc_client_lwt.Client

let debug = false

let server = "ulminfo.fr"
let port = 3724

let realname = "Nethack Bot, 3rd Edition, Version 0.577"
let nick = "nhb"
let username = nick
let channel =
  if debug
  then "#test"
  else "#ulminfo"
let target = 
  if debug 
  then "Ctl-Maj-5cfb"
  else channel

let rec increasing_diff oldL newL =
  match (oldL, newL) with
  | ([], l2) -> l2
  | (h1 :: q1, h2 :: q2) ->
     if h1 = h2
     then increasing_diff q1 q2
     else h2 :: increasing_diff oldL q2
  | (_ :: _, []) -> (Printf.fprintf Pervasives.stderr "New log file lacks some lines" ; [])


let unwrap b =
  match b with
  | Some x -> x
  | None -> failwith "No value"



let watch_file file callback =
  Lwt_io.with_file ~mode:Lwt_io.Input file
  (fun channel ->
  (Lwt_io.read_lines channel |> Lwt_stream.to_list)
  >>= fun lines -> return (Lwt_mvar.create lines))
  >>= fun lines ->
  let rec loop () =
    sleep 5.0
    >>= fun () -> Lwt_io.with_file ~mode:Lwt_io.Input file
	(fun channel ->
	 Lwt_io.read_lines channel |> Lwt_stream.to_list
	 >>= fun nLines ->  Lwt_mvar.take lines
	 >>= fun oLines -> Lwt_mvar.put lines nLines
	 >>= fun () -> Lwt_list.iter_s callback (increasing_diff oLines nLines))
    >>= loop
  in loop ()


let send_message connection line =
  let open Nethack in
  let info = parse_log_line line in
  let message =
    Printf.sprintf "%s, a %s %s %s %s, ended on level %d [max %d], %s, with %d hit points out of %d and after %d deaths, scoring %d. %s."
		   info.pseudo
		   (print_alignement info.alignement)
		   (print_gender info.gender)
		   (print_race info.race)
		   (print_role info.role)
		   info.dungeon_level
		   info.maximum_level
		   (print_dungeon info.dungeon)
		   info.hit_points
		   info.maximum_hit_points
		   info.number_death
		   info.score
		   (String.capitalize info.cause)
  in
  Irc.send_privmsg ~connection ~target ~message

let ping_server connection () =
  Irc.listen ~connection ~callback:(fun ~connection ~result -> return ())


let lwt_main =
  Irc.connect_by_name ~server ~port ~username ~mode:0 ~realname ~nick ()
  >>= fun connection -> return (unwrap connection)
  >>= fun connection -> return (Lwt_daemon.daemonize ())
  >>= fun () -> sleep 10.0
  >>= fun () -> Irc.send_join ~connection ~channel
  >>= fun () -> join [watch_file "/usr/local/util/packages/nethack/share/logfile" (send_message connection) ;
		      ping_server connection ()]

let _ = Lwt_main.run lwt_main
