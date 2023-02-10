open Lwt.Infix

let ( / ) = Filename.concat

let fetch ~log:_ ~rootfs base =
  let base = "/Users" / base in
  let zfs_dir = String.sub rootfs 0 (String.length rootfs - 7) in  (* remove /rootfs from the end *)
  let zfs_volume = String.sub rootfs 9 (String.length rootfs - 16) in  (* remove /Volume/ from front and /rootfs from the end *)
  Os.sudo [ "zfs"; "create"; zfs_volume / "home" ] >>= fun () ->
  Os.sudo [ "zfs"; "create"; zfs_volume / "local" ] >>= fun () ->
  Macos.copy_template ~base:(base / "home") ~local:(zfs_dir / "home") >>= fun _ ->
  Os.sudo [ "chown"; "-R"; ":1000"; zfs_dir / "home" ] >>= fun () ->
  Os.sudo [ "chmod"; "-R"; "g+w"; zfs_dir / "home" ] >>= fun () ->
  Macos.copy_template ~base:(base / "local") ~local:(zfs_dir / "local") >>= fun _ ->
  Os.sudo [ "chown"; "-R"; ":1000"; zfs_dir / "local" ] >>= fun () ->
  Os.sudo [ "chmod"; "-R"; "g+w"; zfs_dir / "local" ] >>= fun () ->
  Lwt.return []
