open Mirage


let persist_remote =
  let default = false in
  Key.create "persist-remote" @@ Key.Arg.opt Key.Arg.bool default (Key.Arg.info ["persist-remote"])


let addr = Ipaddr.V4.of_string_exn

let persist_host =
  let default = addr "10.0.0.1" in
  Key.create "persist-host" @@ Key.Arg.opt Key.Arg.ipv4 default (Key.Arg.info ["persist-host"])

let persist_port =
  let default = 10002 in
  Key.create "persist-port" @@ Key.Arg.opt Key.Arg.int default (Key.Arg.info ["persist-port"])


let keys = Key.[
  abstract persist_remote;
  abstract persist_host;
  abstract persist_port; ]


let stack =
  if_impl Key.is_xen
    (direct_stackv4_with_default_ipv4 default_console (netif "0"))
    (socket_stackv4 default_console [Ipaddr.V4.any])


let conduit_impl = conduit_direct stack


let http_impl = http_server @@ conduit_direct stack


let resolver_impl =
  if_impl Key.is_xen (resolver_dns stack) resolver_unix_system


let main =
  foreign "Unikernel.Main"
    (http @-> resolver @-> conduit @-> clock @-> job)

let () =
  let libraries = [
      "logs";
      "mirage-logs";
      "pih-store"
    ] in
  register ~libraries ~keys "review" [
    main $ http_impl $ resolver_impl $ conduit_impl $ default_clock
  ]
