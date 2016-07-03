open Mirage

let stack =
  match get_mode () with
  | `Xen -> generic_stackv4 default_console tap0
  | _ -> socket_stackv4 default_console [Ipaddr.V4.any]

let keys = generic_kv_ro "tls"

let conduit_direct = conduit_direct ~tls:true stack
let https = http_server @@ conduit_direct

let main =
  foreign "Unikernel.Main"
    (http @-> resolver @-> conduit @-> kv_ro @-> clock @-> job)

let tracing = mprof_trace ~size:1000000 ()

let () =
  let libraries = [
      "logs";
      "mirage-logs";
      "git";
      "irmin.mirage";
      "decompress";
      "uuidm";
    ] in
  register ~libraries "catalog" [
    main $ https $ resolver_dns stack $ conduit_direct $ keys $ default_clock
  ]
