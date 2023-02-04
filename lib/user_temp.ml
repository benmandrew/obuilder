open Lwt.Infix

let fetch ~log:_ ~rootfs base =
  let base = Filename.concat "/Users" base in
  let zfs_dir = String.sub rootfs 0 (String.length rootfs - 7) in  (* remove /rootfs from the end *)
  let zfs_volume = String.sub rootfs 9 (String.length rootfs - 16) in  (* remove /Volume/ from front and /rootfs from the end *)
  let zfs_home_dir = Filename.concat zfs_volume "home" in
  let zfs_local = Filename.concat zfs_volume "local" in
  Os.sudo [ "zfs"; "create"; zfs_home_dir ] >>= fun () ->
  Os.sudo [ "zfs"; "create"; zfs_local ] >>= fun () ->
  Macos.copy_template ~base:(Filename.concat base "home") ~local:(Filename.concat zfs_dir "home") >>= fun _ ->
  Os.sudo [ "chown"; "-R"; ":1000"; Filename.concat zfs_dir "home" ] >>= fun () ->
  Os.sudo [ "chmod"; "-R"; "g+w"; Filename.concat zfs_dir "home" ] >>= fun () ->
  Macos.copy_template ~base:(Filename.concat base "local") ~local:(Filename.concat zfs_dir "local") >>= fun _ ->
  Os.sudo [ "chown"; "-R"; ":1000"; Filename.concat zfs_dir "local" ] >>= fun () ->
  Os.sudo [ "chmod"; "-R"; "g+w"; Filename.concat zfs_dir "local" ] >>= fun () ->
  Lwt.return []
