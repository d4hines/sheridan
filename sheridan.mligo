type sha = bytes
type level = nat

type commitment_map = (address, sha) map
type commitment_storage = (level, commitment_map) big_map

type reveal_map = (address, bytes) map
type reveal_storage = (level, reveal_map) big_map

type members = address set

type storage = members * commitment_storage * reveal_storage

let interaction_time = 5n

let assert_level_is_open (level : level) : unit =
  assert ( level >= Tezos.level && level <= Tezos.level + interaction_time)

let assert_valid_sha (bytes : bytes) : unit =
  assert (Bytes.length bytes = 32n)

let find_commitment_map_for_level (level : level) (commitment_storage) : commitment_map
  match Big_map.find_opt level commitment_storage with
  | Some commitment_map -> 
    if Map.mem Tezos.sender commitment_map then
    (failwith "You already committed for this level" : commitment_map)
    else
      Map.add Tezos.sender sha commitment_map
  | None -> ""

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
  | None -> (Map.empty : commitment_map)
  in
  Big_map.add level new_level commitment_storage


let no_commit_found_msg = 
"You did not commit for this level, and thus can't reveal"

let reveal 
  (level : level)
  (bytes : bytes)
  (commitment_storage : commitment_storage)
  (reveal_storage : reveal_storage)
  : reveal_storage =
  let _ = assert ((Bytes.length bytes) = 64n) in
  let new_level = match Big_map.find_opt level commitment_storage with
  | Some commitment_map -> 
    (
      match Map.find_opt Tezos.sender commitment_map with
      | Some sha -> 
        let calculated_sha = Crypto.sha256 bytes in
        let _ = assert (sha = calculated_sha) in
        (failwith "foo" : reveal_map)
      | None ->
        (failwith no_commit_found_msg : reveal_map)
    )
  | None ->
    (failwith no_commit_found_msg : reveal_map)
  in
  Big_map.add level new_level reveal_storage

let main (_parameter, storage : unit * storage) : operation list * storage =
  (([] : operation list), storage)
