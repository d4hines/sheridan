type sha = bytes
type level = nat

type commitment_map = (address, sha) map
type commitment_storage = (level, commitment_map) big_map

type reveal_map = (address, nat) map
type reveal_storage = (level, reveal_map) big_map

type members = address set

type storage = members * commitment_storage * reveal_storage

let interaction_time = 5n

let assert_level_is_open (level : level) : unit =
  assert ( level >= Tezos.level && level <= Tezos.level + interaction_time)

let assert_valid_sha (bytes : bytes) : unit =
  assert (Bytes.length bytes = 32n)

let commit
  (level : level)
  (sha : sha)
  (commitment_storage : commitment_storage) : commitment_storage =
  let _ = assert_valid_sha sha in
  let _ = assert_level_is_open level in
  let new_level = match Big_map.find_opt level commitment_storage with
  | Some commitment_map -> 
    if Map.mem Tezos.sender commitment_map then
    (failwith "You already committed for this level" : commitment_map)
    else
      Map.add Tezos.sender sha commitment_map
  | None -> (Map.add Tezos.sender sha (Map.empty : commitment_map))
  in
  Big_map.add level new_level commitment_storage

  let no_commit_found_msg = 
"You did not commit for this level, and thus can't reveal"

let assert_level_is_pending (level: level) : unit = 
  assert (level < Tezos.level && level >= Tezos.level + interaction_time)

let reveal 
  (level : level)
  (n : nat)
  (commitment_storage : commitment_storage)
  (reveal_storage : reveal_storage)
  : reveal_storage =
  let bytes = Bytes.pack n in
  let _ = assert ((Bytes.length bytes) = 64n) in
  let _ = assert_level_is_pending level in
  let reveal_level = match Big_map.find_opt level commitment_storage with
  | Some commitment_map -> 
    (
      match Map.find_opt Tezos.sender commitment_map with
      | Some sha -> 
        let calculated_sha = Crypto.sha256 bytes in
        let _ = assert (sha = calculated_sha) in
        let prev = (match Big_map.find_opt level reveal_storage with
        | Some reveals -> reveals
        | None -> (Map.empty : reveal_map))
        in
        Map.add Tezos.sender n prev
      | None ->
        (failwith no_commit_found_msg : reveal_map)
    )
  | None ->
    (failwith no_commit_found_msg : reveal_map)
    in 
    Big_map.add level reveal_level reveal_storage

type op_kind = 
| Commit of sha
| Reveal of nat

type parameter = level * op_kind

let remove_old_level (commitment_storage : commitment_storage) (reveal_storage: reveal_storage) =
    let old_level = Tezos.level - (interaction_time + 1n) in
    let _ = assert (old_level >= 0) in
    let old_level = abs old_level in
    let commitment_storage = Big_map.remove old_level commitment_storage in
    let reveal_storage = Big_map.remove old_level reveal_storage in
    (commitment_storage, reveal_storage)

let main ((level, op), (members, commitment_storage, reveal_storage) : parameter * storage) : operation list * storage =
  let _ = assert (Set.mem Tezos.sender members) in
  let (commitment_storage, reveal_storage) = remove_old_level commitment_storage reveal_storage in
  let storage = (match op with
  | Commit sha ->
    let commitment_storage = commit level sha commitment_storage in
    (members, commitment_storage, reveal_storage)
  | Reveal n ->
    let reveal_storage = reveal level n commitment_storage reveal_storage in
    (members, commitment_storage, reveal_storage))
  in
  (([] : operation list), storage)
    
type view_params = level * address set 

[@view]
let view_number ((view_params, storage) : (view_params * storage)) : nat option = 
  let (_, _, reveal_storage) = storage in 
  let (level, trusted_addresses) = view_params in
  match Big_map.find_opt level reveal_storage with
  | Some reveals ->
    let f (acc, addr : nat option * address) = 
    (match acc with
    | Some acc ->
      (match Map.find_opt addr reveals with
       | Some n -> Some (Bitwise.shift_right (acc + n) 1n)
       | None -> None)
    | None -> (None : nat option)
    )
    in
    Set.fold f trusted_addresses (Some 0n)
  | None -> None
